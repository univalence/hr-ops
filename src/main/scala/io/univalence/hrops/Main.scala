package io.univalence.hrops

import io.univalence.hrops.PDF.DatabaseEntry
import io.univalence.hrops.PDF.DatabaseEntry._
import io.univalence.hrops.PDF.PDFOperation.{AddPDFs, AddTable, CreateNewPDF, SavePDF}

import zio._
import zio.notion._
import zio.notion.dsl._
import zio.notion.model.database.query._
import zio.stream.ZSink

import java.io.ByteArrayOutputStream

object Main extends ZIOAppDefault {

  val filter: Filter = $"Tag".asSelect.equals("filter-1")

  def download(uri: String): ZIO[Downloader, Nothing, Array[Byte]] = {
    val byteArrayOutputStream: ByteArrayOutputStream = new ByteArrayOutputStream()
    for {
      _ <-
        Downloader
          .download(uri)
          .run(ZSink.fromOutputStream(byteArrayOutputStream))
          .orDie
    } yield byteArrayOutputStream.toByteArray
  }

  def program(): ZIO[Downloader with Notion, NotionError, Unit] =
    for {
      database <- Notion.queryAllDatabase(Constants.dbId, filter)
      effects = database.results.map(_.propertiesAs[DatabaseEntry].toZIO)
      entries <- ZIO.collectAllPar(effects)
      downloadEffects =
        entries.flatMap { entry =>
          entry.pdf.collect {
            case attachment: PDF.Attachment if attachment.name.split("\\.").last.equals("pdf") =>
              download(attachment.url)
          }
        }

      pdfs <- ZIO.collectAllPar(downloadEffects)
      documentDescription = CreateNewPDF >> AddTable(entries) >> AddPDFs(pdfs) >> SavePDF("concat")
      _ <- documentDescription.interpret.orDie

    } yield ()

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    program()
      .tapError(err => Console.printLine(err.humanize))
      .provide(Downloader.live, Notion.layerWith(Constants.bearer))
}
