package io.univalence.hrops

import io.univalence.hrops.PDF.PDFOperation._

import zio._
import zio.notion._
import zio.notion.dsl._
import zio.notion.model.database.query._

object Main extends ZIOAppDefault {

  val filter: Filter = $"Tag".asSelect.equals("filter-1")

  def program: ZIO[Downloader with Notion, Throwable, Unit] =
    for {
      database <- Notion.queryAllDatabase(Constants.dbId, filter)
      destructureEffects = database.results.map(_.propertiesAs[DatabaseEntry].toZIO)
      entries <- ZIO.collectAllPar(destructureEffects)
      contextualizeEffects = entries.map(_.contextualize)
      documentContexts <- ZIO.collectAllPar(contextualizeEffects)
      flattenDocumentContexts = documentContexts.flatten
      documentDescription =
        CreateNewPDF >>
          AddTable(flattenDocumentContexts) >> AddPDFs(flattenDocumentContexts) >> Paginate >> SavePDF("concat")
      _ <- documentDescription.interpret.orDie
    } yield ()

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    program
      .provide(Downloader.live, Notion.layerWith(Constants.bearer))
}
