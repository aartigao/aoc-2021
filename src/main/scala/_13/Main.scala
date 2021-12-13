package _13

import common.Runner
import zio._
import zio.console._
import zio.stream._

import scala.util.matching.Regex

object Main extends Runner {

  case class Dot(x: Int, y: Int)

  object Dot {

    val regex: Regex = """(\d+),(\d+)""".r

    def parse(line: String): Option[Dot] =
      line match {
        case regex(x, y) => Some(Dot(x.toInt, y.toInt))
        case _           => None
      }

  }

  case class Matrix(dots: Set[Dot]) extends AnyVal {

    def fold(fold: Fold): Matrix =
      fold match {
        case FoldLeft(x) =>
          Matrix(
            dots.collect {
              case dot if dot.x < x => dot
              case dot if dot.x > x => dot.copy(x = x - (dot.x - x))
            }
          )
        case FoldUp(y)   =>
          Matrix(
            dots.collect {
              case dot if dot.y < y => dot
              case dot if dot.y > y => dot.copy(y = y - (dot.y - y))
            }
          )
      }

    def size: Int = dots.size

    def display(empty: Char, dot: Char): Iterable[String] = {
      val xs = 0 to dots.maxBy(_.x).x
      val ys = 0 to dots.maxBy(_.y).y
      ys.map(y => xs.foldLeft("")((accum, x) => accum + (if (dots(Dot(x, y))) dot else empty)))
    }

  }

  sealed trait Fold
  case class FoldLeft(x: Int) extends Fold
  case class FoldUp(y: Int)   extends Fold

  object Fold {

    val regex: Regex = """fold along ([xy])=(\d+)""".r

    def parse(line: String): Option[Fold] =
      line match {
        case regex(coord, x) if coord == "x" => Some(FoldLeft(x.toInt))
        case regex(coord, y) if coord == "y" => Some(FoldUp(y.toInt))
        case _                               => None
      }

  }

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    input.partition(Dot.regex.matches(_)).use { case (inputDots, inputFolds) =>
      for {
        dots  <- inputDots.mapConcat(Dot.parse).runCollect
        folds <- inputFolds.mapConcat(Fold.parse).runCollect
        matrix = Matrix(dots.toSet)
        _     <- putStrLn(matrix.fold(folds.head).size.toString)
      } yield ()
    }

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    input.partition(Dot.regex.matches(_)).use { case (inputDots, inputFolds) =>
      for {
        dots  <- inputDots.mapConcat(Dot.parse).runCollect
        folds <- inputFolds.mapConcat(Fold.parse).runCollect
        matrix = folds.foldLeft(Matrix(dots.toSet))(_ fold _)
        _     <- ZIO.foreach_(matrix.display('.', '#'))(line => putStrLn(line))
      } yield ()
    }

}
