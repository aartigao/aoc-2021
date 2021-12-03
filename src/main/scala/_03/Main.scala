package _03

import common.Runner
import zio._
import zio.console._
import zio.stream._

import scala.annotation.tailrec

object Main extends Runner {

  case class State(ones: Seq[Int], total: Int) {

    def update(line: String): State =
      State(
        line.zipAll(ones, '1', 0).map {
          case ('1', counter) => counter + 1
          case (_, counter)   => counter
        },
        total + 1
      )

    def gammaRate: Int = Integer.parseInt(ones.map(counter => if (total - counter < counter) '1' else '0').mkString, 2)

    def epsilonRate: Int = ~gammaRate & (-1 >>> (32 - ones.size))

    def powerConsumption: Long = gammaRate * epsilonRate

  }

  case class Digits(zeroes: Int, ones: Int) {

    def update(char: Char): Digits =
      char match {
        case '0' => Digits(zeroes + 1, ones)
        case '1' => Digits(zeroes, ones + 1)
        case _   => this
      }

    def mostCommonValue(onEquals: Char): Char =
      if (zeroes == ones) onEquals
      else if (zeroes > ones) '0'
      else '1'

    def leastCommonValue(onEquals: Char): Char =
      if (zeroes == ones) onEquals
      else if (zeroes < ones) '0'
      else '1'

  }

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    input
      .fold(State(Seq.empty, 0))(_.update(_))
      .tap(state => putStrLn(state.powerConsumption.toString))
      .unit

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      elements          <- input.runCollect
      lifeSupportRating <- oxygenGeneratorRating(elements).zipWithPar(co2ScrubberRating(elements))(_ * _)
      _                 <- putStrLn(lifeSupportRating.toString)
    } yield ()

  def oxygenGeneratorRating(elements: Seq[String]): UIO[Int] = {

    @tailrec
    def loop(pos: Int, elems: Seq[String]): Int =
      if (elems.size == 1) Integer.parseInt(elems.head, 2)
      else {
        val digits = elems.foldLeft(Digits(0, 0))((state, elem) => state.update(elem.charAt(pos)))
        val mcv    = digits.mostCommonValue('1')
        loop(pos + 1, elems.filter(_.charAt(pos) == mcv))
      }

    UIO(loop(0, elements))
  }

  def co2ScrubberRating(elements: Seq[String]): UIO[Int] = {

    @tailrec
    def loop(pos: Int, elems: Seq[String]): Int =
      if (elems.size == 1) Integer.parseInt(elems.head, 2)
      else {
        val digits = elems.foldLeft(Digits(0, 0))((state, elem) => state.update(elem.charAt(pos)))
        val lcv    = digits.leastCommonValue('0')
        loop(pos + 1, elems.filter(_.charAt(pos) == lcv))
      }

    UIO(loop(0, elements))
  }

}
