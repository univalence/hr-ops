package io.univalence.hrops

import zio._
import zio.stream._

import java.io.IOException
import java.net.URL

trait Downloader {
  def download(url: String): Stream[IOException, Byte]
}

object Downloader {

  def download(url: String): ZStream[Downloader, IOException, Byte] =
    ZStream.serviceWithStream[Downloader](_.download(url))

  private case class HttpClientLive() extends Downloader {

    def download(url: String): Stream[IOException, Byte] =
      ZStream.fromInputStreamZIO(
        ZIO.logInfo(s"Downloading a file from $url") *>
          ZIO.attemptBlocking(new URL(url).openStream()).refineToOrDie
      )
  }

  val live: ULayer[Downloader] = ZLayer.succeed(new HttpClientLive)
}
