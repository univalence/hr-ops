package io.univalence.hrops

import be.quodlibet.boxable.BaseTable
import org.apache.pdfbox.multipdf.PDFMergerUtility
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream}
import org.apache.pdfbox.pdmodel.PDPageContentStream.AppendMode
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.font.PDType1Font

import zio._

import java.awt.Color
import java.io.ByteArrayOutputStream

object PDF {

  final case class DocumentContext(name: String, bytes: Array[Byte], numberOfPage: Int, databaseRow: DatabaseEntry)

  sealed trait PDFOperation { self =>
    import PDFOperation._
    def flatMap(function: PDFOperation => PDFOperation): PDFOperation = AndThen(self, function(self))
    def >>(other: PDFOperation): AndThen                              = AndThen(self, other)
  }

  object PDFOperation {

    case object CreateNewPDF                                               extends PDFOperation
    case object Paginate                                                   extends PDFOperation
    case class SavePDF(name: String)                                       extends PDFOperation
    case class AddTable(data: Seq[DocumentContext])                        extends PDFOperation
    case class AddPDF(pdf: DocumentContext)                                extends PDFOperation
    case class AddPDFs(pdfs: Seq[DocumentContext])                         extends PDFOperation
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
          def addTitle(title: String, contentStream: PDPageContentStream): Task[Unit] =
            ZIO.attempt {
              contentStream.beginText()
              contentStream.setFont(PDType1Font.HELVETICA_BOLD, 22)
              contentStream.newLineAtOffset(50, 700)
              contentStream.showText(title)
              contentStream.endText()
            }
          def createHeader(table: BaseTable): Task[Unit] =
            ZIO.attempt {
              val headerRow = table.createRow(15f)
              headerRow.createCell(40, "Name")
              headerRow.createCell(30, "Document")
              headerRow.createCell(20, "Author")
              headerRow.createCell(10, "Page")
              table.addHeaderRow(headerRow)
            }

          def addTableRow(documentContext: DocumentContext, table: BaseTable, pageCursor: Int) =
            ZIO.attempt {
              val row       = table.createRow(12)
              val newCursor = pageCursor + 1
              row.createCell(40, documentContext.databaseRow.name)
              row.createCell(30, documentContext.name)
              row.createCell(20, documentContext.databaseRow.author)
              row.createCell(10, newCursor.toString)
              documentContext.numberOfPage + pageCursor
            }

          def drawTable(table: BaseTable, contentStream: PDPageContentStream, document: PDDocument, page: PDPage) =
            ZIO.attempt {
              table.draw()
              contentStream.close()
              document.addPage(page)
            }

          val byteArrayOutputStream = new ByteArrayOutputStream()

          for {
            loadedDocument <- ZIO.attempt(PDDocument.load(document))
            newPage       = new PDPage(PDRectangle.A4)
            yStartNewPage = newPage.getMediaBox.getHeight - (2 * config.margin)
            tableWidth    = newPage.getMediaBox.getWidth - (2 * config.margin)
            contentStream = new PDPageContentStream(loadedDocument, newPage)
            _ <- addTitle("Résumé", contentStream)
            table =
              new BaseTable(
                config.yPosition,
                yStartNewPage,
                config.bottomMargin,
                tableWidth,
                config.margin,
                loadedDocument,
                newPage,
                config.drawLines,
                config.drawContent
              )
            _ <- createHeader(table)
            _ <- ZIO.foldLeft(data)(0)((acc, documentContext) => addTableRow(documentContext, table, acc))
            _ <- drawTable(table, contentStream, loadedDocument, newPage)
            _ <-
              ZIO.attempt {
                loadedDocument.save(byteArrayOutputStream)
                loadedDocument.close()
              }
          } yield byteArrayOutputStream.toByteArray

        case AddPDF(pdf) =>
          val pdfMerger = new PDFMergerUtility()
          for {
            documentToBeMerged <- ZIO.attempt(PDDocument.load(pdf.bytes))
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

        case Paginate =>
          val byteArrayOutputStream = new ByteArrayOutputStream()
          def addPageNumber(number: Int, contentStream: PDPageContentStream) =
            ZIO.attempt {
              contentStream.beginText()
              contentStream.setFont(PDType1Font.HELVETICA_BOLD, 22)
              contentStream.newLineAtOffset(18, 18)
              contentStream.showText(number.toString)
              contentStream.endText()
              contentStream.addRect(10, 10, 30, 30)
              contentStream.setStrokingColor(Color.BLACK)
              contentStream.stroke()
            }

          for {
            loadedDocument <- ZIO.attempt(PDDocument.load(document))
            numberOfPages  <- ZIO.attempt(loadedDocument.getNumberOfPages)
            effects: Seq[ZIO[Any, Throwable, Unit]] =
              (0 until numberOfPages).map(i =>
                for {
                  PDPage <- ZIO.attempt(loadedDocument.getPage(i))
                  contentStream = new PDPageContentStream(loadedDocument, PDPage, AppendMode.APPEND, false)
                  _ <- addPageNumber(i, contentStream)
                  _ <- ZIO.attempt(contentStream.close())
                } yield ()
              )
            _ <- ZIO.collectAll(effects)
            _ <-
              ZIO.attempt {
                loadedDocument.save(byteArrayOutputStream)
                loadedDocument.close()
              }

          } yield byteArrayOutputStream.toByteArray
      }
  }
}
