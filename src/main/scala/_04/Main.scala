package _04

import common.Runner
import zio._
import zio.console._
import zio.stream._

import scala.annotation.tailrec

object Main extends Runner {

  case class Draw(numbers: Seq[Int]) extends AnyVal

  case class BoardNumber(value: Int, marked: Boolean = false) {

    def mark: BoardNumber = BoardNumber(value, marked = true)

  }

  case class Board(numbers: IndexedSeq[BoardNumber]) extends AnyVal {

    def update(value: Int): Board =
      Board(
        numbers.map(number => if (number.value == value) number.mark else number)
      )

    def winner: Boolean = {

      def row(idx: Int): Boolean = Range(idx * 5, (idx * 5) + 5).forall(numbers(_).marked)

      def col(idx: Int): Boolean = Range(idx, idx + 21, 5).forall(numbers(_).marked)

      (0 until 5).exists(idx => row(idx) || col(idx))
    }

  }

  private def parseDraw(line: String): Draw = Draw(line.split(',').map(_.toInt))

  private def parseBoards(lines: Seq[String]): Seq[Board] = {

    def parseBoardLine(line: String): Seq[Int] = line.split("\\s+").filter(_.nonEmpty).map(_.toInt)

    lines
      .grouped(5)
      .map(raw =>
        Board(
          raw
            .flatMap(parseBoardLine)
            .map(value => BoardNumber(value))
            .toIndexedSeq
        )
      )
      .toSeq
  }

  private def score(drawNumber: Int, winnerBoard: Board): Int =
    drawNumber * winnerBoard.numbers.filterNot(_.marked).map(_.value).sum

  def scoreFirstWinner(draw: Draw, boards: Iterable[Board]): Int = {

    @tailrec
    def firstWinnerBoard(remaining: Draw, boards: Iterable[Board]): Int = {
      val drawNumber    = remaining.numbers.head
      val updatedBoards = boards.map(_.update(drawNumber))
      updatedBoards.find(_.winner) match {
        case Some(winnerBoard) => score(drawNumber, winnerBoard)
        case None              => firstWinnerBoard(Draw(remaining.numbers.tail), updatedBoards)
      }
    }

    firstWinnerBoard(draw, boards)
  }

  def scoreLastWinner(draw: Draw, boards: Iterable[Board]): Int = {

    @tailrec
    def lastWinnerBoard(remaining: Draw, boards: Iterable[Board]): Int = {
      val drawNumber        = remaining.numbers.head
      val updatedBoards     = boards.map(_.update(drawNumber))
      val (winners, losers) = updatedBoards.partition(_.winner)
      if (losers.isEmpty) score(drawNumber, winners.last)
      else lastWinnerBoard(Draw(remaining.numbers.tail), losers)
    }

    lastWinnerBoard(draw, boards)
  }

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      lines <- input.filter(_.nonEmpty).runCollect
      draw   = parseDraw(lines.head)
      boards = parseBoards(lines.tail)
      score  = scoreFirstWinner(draw, boards)
      _     <- putStrLn(score.toString)
    } yield ()

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      lines <- input.filter(_.nonEmpty).runCollect
      draw   = parseDraw(lines.head)
      boards = parseBoards(lines.tail)
      score  = scoreLastWinner(draw, boards)
      _     <- putStrLn(score.toString)
    } yield ()

}
