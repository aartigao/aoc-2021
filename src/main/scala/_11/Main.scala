package _11

import common.Runner
import zio._
import zio.console._
import zio.stream._

object Main extends Runner {

  case class Flashes(count: Int) extends AnyVal {

    def +(other: Flashes): Flashes = Flashes(count + other.count)

  }

  object Flashes {

    val empty: Flashes = Flashes(0)

  }

  case class Octopus(energy: Int) extends AnyVal {

    def inc: Octopus = Octopus(energy + 1)

    def hasFlashed: Boolean = energy == 10

    def isFlashed: Boolean = energy > 9

  }

  object Octopus {

    val drained: Octopus = Octopus(0)

  }

  case class OctopusGrid(grid: Array[Array[Octopus]]) {

    private val rows = grid.length
    private val cols = grid.headOption.getOrElse(Array.empty).length

    val size: Long = rows * cols

    def step: Flashes = {
      increaseAll()
      val drained = drainFlashed
      Flashes(drained)
    }

    private def increaseAll(): Unit =
      for {
        row <- 0 until rows
        col <- 0 until cols
      } yield increaseAndFlash(row, col)

    private def increaseAndFlash(row: Int, col: Int): Unit = {
      val increased = grid(row)(col).inc
      grid(row)(col) = increased
      if (increased.hasFlashed) flashNeighbors(row, col)
    }

    private def flashNeighbors(row: Int, col: Int): Unit = {
      if (row > 0 && col > 0) increaseAndFlash(row - 1, col - 1)               // Top left
      if (row > 0) increaseAndFlash(row - 1, col)                              // Top
      if (row > 0 && col < cols - 1) increaseAndFlash(row - 1, col + 1)        // Top right
      if (col > 0) increaseAndFlash(row, col - 1)                              // Left
      if (col < cols - 1) increaseAndFlash(row, col + 1)                       // Right
      if (row < rows - 1 && col > 0) increaseAndFlash(row + 1, col - 1)        // Bottom left
      if (row < rows - 1) increaseAndFlash(row + 1, col)                       // Bottom
      if (row < rows - 1 && col < cols - 1) increaseAndFlash(row + 1, col + 1) // Bottom right
    }

    private def drainFlashed: Int =
      (for {
        row <- 0 until rows
        col <- 0 until cols
      } yield {
        val flashed = grid(row)(col).isFlashed
        if (flashed) grid(row)(col) = Octopus.drained
        flashed
      }).count(identity)

  }

  object OctopusGrid {

    def make(input: Array[Array[Int]]): OctopusGrid =
      OctopusGrid(input.map(_.map(Octopus.apply)))

  }

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      grid    <- input
                   .take(10)
                   .map(_.toCharArray.map(_.toString.toInt))
                   .runCollect
                   .map(chunk => OctopusGrid.make(chunk.toArray))
      flashes <- ZStream
                   .repeat(grid.step)
                   .take(100)
                   .fold(Flashes.empty)(_ + _)
      _       <- putStrLn(flashes.count.toString)
    } yield ()

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      grid <- input
                .take(10)
                .map(_.toCharArray.map(_.toString.toInt))
                .runCollect
                .map(chunk => OctopusGrid.make(chunk.toArray))
      step <- ZStream
                .range(1, Int.MaxValue)
                .zip(ZStream.repeat(grid.step))
                .filter(_._2.count == grid.size)
                .map(_._1)
                .runHead
                .someOrFailException
      _    <- putStrLn(step.toString)
    } yield ()

}
