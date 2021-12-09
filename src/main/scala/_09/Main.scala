package _09

import common.Runner
import zio._
import zio.console._
import zio.stream._

object Main extends Runner {

  case class Point(row: Int, col: Int)

  case class Basin(points: Set[Point]) extends AnyVal {

    def +(point: Point): Basin = Basin(points + point)

    def ++(other: Basin): Basin = Basin(points ++ other.points)

  }

  object Basin {

    val empty: Basin = Basin(Set.empty)

  }

  case class Visited(points: Set[Point]) extends AnyVal {

    def +(point: Point): Visited = Visited(points + point)

    def contains(point: Point): Boolean = points(point)

  }

  object Visited {

    val empty: Visited = Visited(Set.empty)

  }

  case class HeightmapMatrix(values: Array[Array[Char]]) {

    private val rows = values.length
    private val cols = values.headOption.getOrElse(Array.empty).length

    def pointValue(point: Point): Int = values(point.row)(point.col)

    def isLowPoint(point: Point): Boolean = {

      def up: Char    = if (point.row == 0) '9' else values(point.row - 1)(point.col)
      def down: Char  = if (point.row == rows - 1) '9' else values(point.row + 1)(point.col)
      def left: Char  = if (point.col == 0) '9' else values(point.row)(point.col - 1)
      def right: Char = if (point.col == cols - 1) '9' else values(point.row)(point.col + 1)

      val value = values(point.row)(point.col)
      value < up && value < down && value < left && value < right
    }

    def lowPoints: Seq[Point] =
      for {
        row  <- 0 until rows
        col  <- 0 until cols
        point = Point(row, col)
        if isLowPoint(point)
      } yield point

    def lowPointsValues: Seq[Int] = lowPoints.map(point => values(point.row)(point.col).toString.toInt)

    def basin(lowPoint: Point): Basin = {

      def isHigh(point: Point): Boolean = values(point.row)(point.col) == '9'

      def isOutOfBounds(point: Point): Boolean =
        point.row < 0 || point.row >= rows || point.col < 0 || point.col >= cols

      def search(point: Point, visited: Visited): (Basin, Visited) =
        if (visited.contains(point)) (Basin.empty, visited)
        else if (isOutOfBounds(point) || isHigh(point)) (Basin.empty, visited + point)
        else {
          val upPoint    = Point(point.row - 1, point.col)
          val downPoint  = Point(point.row + 1, point.col)
          val leftPoint  = Point(point.row, point.col - 1)
          val rightPoint = Point(point.row, point.col + 1)

          val currentVisited             = visited + point
          val (upBasin, upVisited)       = search(upPoint, currentVisited)
          val (downBasin, downVisited)   = search(downPoint, upVisited)
          val (leftBasin, leftVisited)   = search(leftPoint, downVisited)
          val (rightBasin, rightVisited) = search(rightPoint, leftVisited)

          (upBasin ++ downBasin ++ leftBasin ++ rightBasin + point, rightVisited)
        }

      search(lowPoint, Visited.empty)._1
    }

  }

  object HeightmapMatrix {

    def apply(input: Chunk[String]): HeightmapMatrix =
      HeightmapMatrix(input.map(_.toCharArray).toArray)

  }

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      matrix    <- input.runCollect.map(HeightmapMatrix.apply)
      riskLevels = matrix.lowPointsValues.map(_ + 1).sum
      _         <- putStrLn(riskLevels.toString)
    } yield ()

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      matrix <- input.runCollect.map(HeightmapMatrix.apply)
      basins <- ZIO.foreachPar(matrix.lowPoints)(lowPoint => UIO(matrix.basin(lowPoint)))
      product = basins.map(_.points.size).sorted.takeRight(3).product
      _      <- putStrLn(product.toString)
    } yield ()

}
