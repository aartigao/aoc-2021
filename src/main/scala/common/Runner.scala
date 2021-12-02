package common

import zio._
import zio.stream._

import java.io.{FileInputStream, IOException}
import scala.io.Source

trait Runner extends App {

  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    (args match {
      case "1" :: path :: Nil => partOne(fileStream(path))
      case "2" :: path :: Nil => partTwo(fileStream(path))
      case "1" :: Nil         => partOne(stdInputStream)
      case "2" :: Nil         => partTwo(stdInputStream)
      case other              => Task.fail(new IllegalArgumentException(s"Invalid input: ${other.mkString(" ")}"))
    }).exitCode

  private def fileStream(path: String): ZStream[Any, Throwable, String] =
    ZStream
      .fromInputStreamEffect(Task.effect(new FileInputStream(path)).refineToOrDie[IOException])
      .transduce(ZTransducer.utfDecode >>> ZTransducer.splitLines)

  private def stdInputStream: ZStream[Any, Throwable, String] = ZStream.fromIterator(Source.stdin.getLines())

  def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit]

  def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit]

}
