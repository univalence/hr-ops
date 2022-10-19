package io.univalence.hrops

import org.apache.pdfbox.pdmodel.PDDocument

import io.univalence.hrops.DatabaseEntry.Attachment
import io.univalence.hrops.Downloader.downloadEffect
import io.univalence.hrops.PDF.DocumentContext

import zio.ZIO
import zio.notion.{NotionColumn, PropertyConverter}
import zio.notion.model.page.Property
import zio.notion.model.page.property.Link
import zio.prelude.Validation

final case class DatabaseEntry(
    @NotionColumn("Name") name:     String,
    @NotionColumn("Author") author: String,
    @NotionColumn("PDF") pdf:       Seq[Attachment]
) { self =>
  def contextualize: ZIO[Downloader, Throwable, Seq[DocumentContext]] = {
    val pdfs: Seq[ZIO[Downloader, Throwable, DocumentContext]] =
      self.pdf.map(attachment =>
        for {
          bytes         <- downloadEffect(attachment.url)
          numberOfPages <- ZIO.attempt(PDDocument.load(bytes).getNumberOfPages)
        } yield DocumentContext(attachment.name, bytes, numberOfPages, self)
      )
    ZIO.collectAllPar(pdfs)
  }
}

object DatabaseEntry {
  final case class Attachment(name: String, url: String)

  implicit val invoice: PropertyConverter[Seq[Attachment]] = {
    case Property.Files(_, files) =>
      Validation.succeed(
        files.collect {
          case Link.External(name, external) => Attachment(name, external.url)
          case Link.File(name, temp)         => Attachment(name, temp.url)
        }
      )
    case _ => Validation.succeed(Seq.empty)
  }
}
