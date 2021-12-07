package _07

import common.Runner
import zio._
import zio.console._
import zio.stream._

object Main extends Runner {

  case class Minimum(position: Int, fuel: Int) {

    def best(other: Minimum): Minimum = if (other.fuel < fuel) other else this

  }

  def fuelCostForOne(from: Int, to: Int): Int = math.abs(from - to)

  def fuelCostForTwo(from: Int, to: Int): Int = (1 to math.abs(from - to)).sum

  def calculate(positions: Chunk[Int], range: Range, fuelCost: (Int, Int) => Int): Int = {

    def totalFuelCost(to: Int): Int = positions.map(from => fuelCost(from, to)).sum

    range.tail
      .foldLeft(Minimum(range.head, totalFuelCost(range.head))) { case (min, to) =>
        min.best(Minimum(to, totalFuelCost(to)))
      }
      .fuel
  }

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      positions <- input
                     .take(1)
                     .flatMap(raw => ZStream.fromIterable(raw.split(',')))
                     .map(_.toInt)
                     .runCollect
      fuel       = calculate(positions, positions.min to positions.max, fuelCostForOne)
      _         <- putStrLn(fuel.toString)
    } yield ()

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      positions <- input
                     .take(1)
                     .flatMap(raw => ZStream.fromIterable(raw.split(',')))
                     .map(_.toInt)
                     .runCollect
      fuel       = calculate(positions, positions.min to positions.max, fuelCostForTwo)
      _         <- putStrLn(fuel.toString)
    } yield ()

}
