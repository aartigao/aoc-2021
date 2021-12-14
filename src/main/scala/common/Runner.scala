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

  protected def occurrences[K](as: Iterable[K]): Map[K, Long] = as.groupMapReduce(identity)(_ => 1L)(_ + _)

  protected def updateSum[K, V: Numeric](map: Map[K, V], k: K, v: V): Map[K, V] =
    update(map, k, v, implicitly[Numeric[V]].plus)

  protected def update[K, V](map: Map[K, V], k: K, v: V, merge: (V, V) => V): Map[K, V] =
    map.updatedWith(k) {
      case Some(value) => Some(merge(value, v))
      case None        => Some(v)
    }

}
