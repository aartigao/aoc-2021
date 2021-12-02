package _02

import common.Runner
import zio._
import zio.console._
import zio.stream._

object Main extends Runner {

  sealed trait Command
  case class Forward(amount: Int) extends Command
  case class Down(amount: Int)    extends Command
  case class Up(amount: Int)      extends Command

  case class StateOne(horizontal: Int, depth: Int) {

    def update(command: Command): StateOne =
      command match {
        case Forward(amount) => StateOne(horizontal + amount, depth)
        case Down(amount)    => StateOne(horizontal, depth + amount)
        case Up(amount)      => StateOne(horizontal, depth - amount)
      }

  }

  case class StateTwo(horizontal: Int, depth: Int, aim: Int) {

    def update(command: Command): StateTwo =
      command match {
        case Forward(amount) => StateTwo(horizontal + amount, depth + (aim * amount), aim)
        case Down(amount)    => StateTwo(horizontal, depth, aim + amount)
        case Up(amount)      => StateTwo(horizontal, depth, aim - amount)
      }

  }

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    input
      .mapM(line => ZIO.fromOption(asCommand(line)).orElseFail(new IllegalArgumentException(s"Invalid line: $line")))
      .fold(StateOne(0, 0))(_.update(_))
      .tap(state => putStrLn((state.horizontal * state.depth).toString))
      .unit

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    input
      .mapM(line => ZIO.fromOption(asCommand(line)).orElseFail(new IllegalArgumentException(s"Invalid line: $line")))
      .fold(StateTwo(0, 0, 0))(_.update(_))
      .tap(state => putStrLn((state.horizontal * state.depth).toString))
      .unit

  def asCommand(input: String): Option[Command] =
    input.split(' ') match {
      case Array("forward", amount) => amount.toIntOption.map(Forward)
      case Array("down", amount)    => amount.toIntOption.map(Down)
      case Array("up", amount)      => amount.toIntOption.map(Up)
      case _                        => None
    }

}
