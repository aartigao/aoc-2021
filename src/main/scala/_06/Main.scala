package _06

import common.Runner
import zio._
import zio.console._
import zio.stream._

import scala.annotation.tailrec

object Main extends Runner {

  case class State(timers: Chunk[Long]) extends AnyVal {

    def shift: State = {
      val reset   = timers.head
      val shifted = timers.tail :+ reset
      State(shifted.updated(6, shifted(6) + reset))
    }

  }

  object State {

    def init(values: Chunk[Int]): State =
      State(
        values.foldLeft(Chunk.fill(9)(0L)) { case (timers, value) => timers.updated(value, timers(value) + 1) }
      )

  }

  private def calculate(days: Int, input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] = {

    @tailrec
    def loop(days: Int, state: State): Long =
      if (days == 0) state.timers.sum
      else loop(days - 1, state.shift)

    for {
      initialState <- input
                        .take(1)
                        .flatMap(s => ZStream.fromIterable(s.split(",")))
                        .map(_.toInt)
                        .runCollect
      total         = loop(days, State.init(initialState))
      _            <- putStrLn(total.toString)
    } yield ()
  }

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    calculate(80, input)

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    calculate(256, input)

}
