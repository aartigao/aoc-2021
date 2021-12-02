package _01

import common.Runner
import zio._
import zio.console._
import zio.stream._

object Main extends Runner {

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    input
      .map(_.toInt)
      .zipWithPrevious
      .filter {
        case (Some(prev), curr) if prev < curr => true
        case _                                 => false
      }
      .runCount
      .tap(count => putStrLn(count.toString))
      .unit

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    input
      .map(_.toInt)
      .zipWithPreviousAndNext
      .collect { case (Some(prev), curr, Some(next)) => prev + curr + next }
      .zipWithPrevious
      .filter {
        case (Some(prev), curr) if prev < curr => true
        case _                                 => false
      }
      .runCount
      .tap(count => putStrLn(count.toString))
      .unit

}
