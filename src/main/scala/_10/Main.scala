package _10

import common.Runner
import zio._
import zio.console._
import zio.stream._

import scala.annotation.tailrec

object Main extends Runner {

  def firstIllegalChar(line: String): Option[Char] = {

    @tailrec
    def loop(remaining: Array[Char], unclosed: List[Char]): Option[Char] =
      if (remaining.isEmpty) None
      else
        (unclosed.headOption, remaining.head) match {
          case (Some('('), ')') => loop(remaining.tail, unclosed.tail)
          case (Some('['), ']') => loop(remaining.tail, unclosed.tail)
          case (Some('{'), '}') => loop(remaining.tail, unclosed.tail)
          case (Some('<'), '>') => loop(remaining.tail, unclosed.tail)
          case (_, ')')         => Some(')')
          case (_, ']')         => Some(']')
          case (_, '}')         => Some('}')
          case (_, '>')         => Some('>')
          case _                => loop(remaining.tail, remaining.head :: unclosed)
        }

    loop(line.toCharArray, List.empty)
  }

  def illegalCharScore(char: Char): Int =
    char match {
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
    }

  def autocomplete(line: String): String = {

    @tailrec
    def loop(remaining: Array[Char], unclosed: List[Char]): String =
      if (remaining.isEmpty)
        unclosed.map {
          case '(' => ')'
          case '[' => ']'
          case '{' => '}'
          case '<' => '>'
        }.mkString
      else
        (unclosed.headOption, remaining.head) match {
          case (Some('('), ')') => loop(remaining.tail, unclosed.tail)
          case (Some('['), ']') => loop(remaining.tail, unclosed.tail)
          case (Some('{'), '}') => loop(remaining.tail, unclosed.tail)
          case (Some('<'), '>') => loop(remaining.tail, unclosed.tail)
          case _                => loop(remaining.tail, remaining.head :: unclosed)
        }

    loop(line.toCharArray, List.empty)
  }

  def autocompleteScore(autocomplete: String): Long =
    autocomplete.collect {
      case ')' => 1L
      case ']' => 2L
      case '}' => 3L
      case '>' => 4L
    }.foldLeft(0L)((totalScore, charScore) => (totalScore * 5L) + charScore)

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      score <- input
                 .map(firstIllegalChar)
                 .collectSome
                 .map(illegalCharScore)
                 .runSum
      _     <- putStrLn(score.toString)
    } yield ()

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      scores <- input
                  .filter(firstIllegalChar(_).isEmpty)
                  .map(autocomplete)
                  .map(autocompleteScore)
                  .runCollect
      middle  = scores.sorted.apply(scores.size / 2)
      _      <- putStrLn(middle.toString)
    } yield ()

}
