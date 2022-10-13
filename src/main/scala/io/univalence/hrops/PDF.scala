package io.univalence.hrops

import be.quodlibet.boxable.BaseTable
import org.apache.pdfbox.multipdf.PDFMergerUtility
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream}
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.font.PDType1Font

import zio._
import zio.notion.{NotionColumn, PropertyConverter}
import zio.notion.model.page.Property
import zio.notion.model.page.property.Link
import zio.prelude.Validation

import java.io.ByteArrayOutputStream

object PDF {

  final case class DatabaseEntry(
      @NotionColumn("Name") name:     String,
      @NotionColumn("Author") author: String,
      @NotionColumn("PDF") pdf:       Seq[Attachment]
  )

  final case class Attachment(name: String, url: String)

  object DatabaseEntry {
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

  sealed trait PDFOperation { self =>
    import PDFOperation._
    def flatMap(function: PDFOperation => PDFOperation): PDFOperation = AndThen(self, function(self))
    def >>(other: PDFOperation): AndThen                              = AndThen(self, other)
  }

  object PDFOperation {

    case object CreateNewPDF                                               extends PDFOperation
    case class SavePDF(name: String)                                       extends PDFOperation
    case class AddTable(data: Seq[DatabaseEntry])                          extends PDFOperation
    case class AddPDF(pdf: Array[Byte])                                    extends PDFOperation
    case class AddPDFs(pdfs: Seq[Array[Byte]])                             extends PDFOperation
    case class AndThen(operation1: PDFOperation, operation2: PDFOperation) extends PDFOperation

    val outputDir = "out/"

    implicit class PDFOperationOps(operation: PDFOperation) {
      def interpret: ZIO[Any, Throwable, Array[Byte]] = interpreter(operation)
    }

    def interpreter(pdfOperation: PDFOperation, document: Array[Byte] = Array.empty): ZIO[Any, Throwable, Array[Byte]] =
      pdfOperation match {
        case CreateNewPDF =>
          val pDDocument            = new PDDocument()
          val byteArrayOutputStream = new ByteArrayOutputStream()
          for {
            _ <- ZIO.attempt(pDDocument.save(byteArrayOutputStream))
            _ <- ZIO.attempt(pDDocument.close())
          } yield byteArrayOutputStream.toByteArray
        case SavePDF(name) =>
          for {
            pDDocument <- ZIO.attempt(PDDocument.load(document))
            _          <- ZIO.attempt(pDDocument.save(s"$outputDir$name.pdf"))
            _          <- ZIO.attempt(pDDocument.close())
          } yield document
        case AddTable(data) =>
          case class TableConfig(
              margin:       Float,
              drawContent:  Boolean,
              bottomMargin: Float,
              yPosition:    Float,
              drawLines:    Boolean = true
          )
          val config = TableConfig(margin = 50, drawContent = true, bottomMargin = 70, yPosition = 550)
          for {
            pDDocument <- ZIO.attempt(PDDocument.load(document))
            newPage       = new PDPage(PDRectangle.A4)
            yStartNewPage = newPage.getMediaBox.getHeight - (2 * config.margin)
            tableWidth    = newPage.getMediaBox.getWidth - (2 * config.margin)
            contentStream = new PDPageContentStream(pDDocument, newPage)
            _ <-
              ZIO.attempt {
                contentStream.beginText()
                contentStream.setFont(PDType1Font.HELVETICA_BOLD, 22)
                contentStream.newLineAtOffset(50, 700)
                contentStream.showText("Résumé")
                contentStream.endText()
              }
            table =
              new BaseTable(
                config.yPosition,
                yStartNewPage,
                config.bottomMargin,
                tableWidth,
                config.margin,
                pDDocument,
                newPage,
                config.drawLines,
                config.drawContent
              )
            headerRow = table.createRow(15f)
            _ <-
              ZIO.attempt {
                headerRow.createCell(50, "Document name")
                headerRow.createCell(50, "Author")
                table.addHeaderRow(headerRow)
              }
            effects =
              data.map { entry =>
                ZIO.attempt {
                  val row = table.createRow(12)
                  row.createCell(50, entry.name)
                  row.createCell(50, entry.author)
                }
              }
            _ <- ZIO.collectAll(effects)

            _ <-
              ZIO.attempt {
                table.draw()
                contentStream.close()
                pDDocument.addPage(newPage)
              }
            byteArrayOutputStream = new ByteArrayOutputStream()
            _ <-
              ZIO.attempt {
                pDDocument.save(byteArrayOutputStream)
                pDDocument.close()
              }
          } yield byteArrayOutputStream.toByteArray
        case AddPDF(pdf) =>
          val pdfMerger = new PDFMergerUtility()
          for {
            documentToBeMerged <- ZIO.attempt(PDDocument.load(pdf))
            originalDocument   <- ZIO.attempt(PDDocument.load(document))
            byteArrayOutputStream = new ByteArrayOutputStream()
            _ <- ZIO.attempt(pdfMerger.appendDocument(originalDocument, documentToBeMerged))
            _ <- ZIO.attempt(originalDocument.save(byteArrayOutputStream))
            _ <-
              ZIO.attempt {
                documentToBeMerged.close()
                originalDocument.close()
              }
          } yield byteArrayOutputStream.toByteArray
        case AddPDFs(pdfs) => ZIO.foldLeft(pdfs)(document)((acc, curr) => interpreter(AddPDF(curr), acc))
        case AndThen(operation1, operation2) =>
          for {
            state0 <- interpreter(operation1, document)
            state1 <- interpreter(operation2, state0)
          } yield state1
      }
  }
}
