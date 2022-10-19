package io.univalence.hrops

import zio._
import zio.stream._

import java.io.{ByteArrayOutputStream, IOException}
import java.net.URL

trait Downloader {
  def download(url: String): Stream[IOException, Byte]
}

object Downloader {

  def download(url: String): ZStream[Downloader, IOException, Byte] =
    ZStream.serviceWithStream[Downloader](_.download(url))

  def downloadEffect(uri: String): ZIO[Downloader, Nothing, Array[Byte]] = {
    val byteArrayOutputStream: ByteArrayOutputStream = new ByteArrayOutputStream()
    for {
      _ <-
        Downloader
          .download(uri)
          .run(ZSink.fromOutputStream(byteArrayOutputStream))
          .orDie
    } yield byteArrayOutputStream.toByteArray
  }

  private case class HttpClientLive() extends Downloader {

    def download(url: String): Stream[IOException, Byte] =
      ZStream.fromInputStreamZIO(
        ZIO.logInfo(s"Downloading a file from $url") *>
          ZIO.attemptBlocking(new URL(url).openStream()).refineToOrDie
      )
  }

  val live: ULayer[Downloader] = ZLayer.succeed(new HttpClientLive)
}
