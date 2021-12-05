package _05

import common.Runner
import zio._
import zio.console._
import zio.stream._

import scala.util.matching.Regex

object Main extends Runner {

  case class Coordinate(x: Int, y: Int) {

    override def toString: String = s"($x,$y)"

  }

  object Coordinate {

    def apply(tuple: (Int, Int)): Coordinate = new Coordinate(tuple._1, tuple._2)

  }

  case class Line(from: Coordinate, to: Coordinate) {

    def horizontal: Boolean = from.y == to.y

    def vertical: Boolean = from.x == to.x

    def diagonal: Boolean = math.abs(from.x - to.x) == math.abs(from.y - to.y)

    def coordinates: Seq[Coordinate] = {
      val xs = Range.inclusive(from.x, to.x, if (from.x < to.x) 1 else -1)
      val ys = Range.inclusive(from.y, to.y, if (from.y < to.y) 1 else -1)
      xs.zipAll(ys, from.x, from.y).map(Coordinate.apply)
    }

    override def toString: String =
      s"$from -> $to: ${coordinates.mkString(" - ")}"

  }

  object Line {

    val range: Regex = """(\d+),(\d+) -> (\d+),(\d+)""".r

    def parse(input: String): Option[Line] =
      input match {
        case range(x1, y1, x2, y2) =>
          Some(
            Line(
              Coordinate(x1.toInt, y1.toInt),
              Coordinate(x2.toInt, y2.toInt)
            )
          )
        case _                     => None
      }

  }

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      lines     <- input
                     .map(Line.parse)
                     .collectSome
                     .runCollect
      overlapped = lines
                     .filter(line => line.horizontal || line.vertical)
                     .flatMap(_.coordinates)
                     .groupBy(identity)
                     .count(_._2.size >= 2)
      _         <- putStrLn(overlapped.toString)
    } yield ()

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      lines     <- input
                     .map(Line.parse)
                     .collectSome
                     .runCollect
      overlapped = lines
                     .filter(line => line.horizontal || line.vertical || line.diagonal)
                     .flatMap(_.coordinates)
                     .groupBy(identity)
                     .count(_._2.size >= 2)
      _         <- putStrLn(overlapped.toString)
    } yield ()

}
