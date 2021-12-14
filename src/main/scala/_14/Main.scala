package _14

import common.Runner
import zio._
import zio.console._
import zio.stream._

import scala.annotation.tailrec
import scala.util.matching.Regex

object Main extends Runner {

  case class Pattern(_1: Char, _2: Char)

  object Pattern {

    def apply(input: String): Pattern = Pattern(input(0), input(1))

  }

  case class Polymer(patterns: Map[Pattern, Long], last: Char) {

    def apply(rules: InsertionRules): Polymer =
      Polymer(
        patterns.iterator.flatMap { case (pattern, times) =>
          val (one, two) = rules(pattern)
          Seq((one, times), (two, times))
        }.foldLeft(Map.empty[Pattern, Long]) { case (accum, (pattern, times)) => updateSum(accum, pattern, times) },
        last
      )

    def sortedOccurrences: Chunk[(Char, Long)] =
      Chunk
        .fromIterable(patterns.foldLeft(Map(last -> 1L)) { case (accum, (Pattern(a, _), times)) =>
          updateSum(accum, a, times)
        })
        .sortBy(_._2)

  }

  object Polymer {

    val regex: Regex = "^([A-Z]+)$".r

    def apply(input: String): Polymer =
      input match {
        case regex(value) => Polymer(occurrences(value.sliding(2).map(Pattern.apply).toSeq), value.last)
        case _            => throw new IllegalArgumentException(s"Invalid template input: $input")
      }

  }

  case class InsertionRules(rules: Map[Pattern, (Pattern, Pattern)]) extends AnyVal {

    def apply(pattern: Pattern): (Pattern, Pattern) = rules(pattern)

  }

  object InsertionRules {

    val regex: Regex = "^([A-Z]{2}) -> ([A-Z])$".r

    def parse(input: Chunk[String]): InsertionRules =
      InsertionRules(
        input.collect {
          case regex(pattern, insertion) =>
            val matching = Pattern(pattern)
            val one      = Pattern(matching._1, insertion.head)
            val two      = Pattern(insertion.head, matching._2)
            (matching, (one, two))
          case line if line.nonEmpty     => throw new IllegalArgumentException(s"Invalid line: $line")
        }.toMap
      )

  }

  def mostCommonMinusLeastCommon(iterations: Int, input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] = {

    @tailrec
    def insertion(iterations: Int, polymer: Polymer, rules: InsertionRules): Polymer =
      if (iterations == 0) polymer
      else insertion(iterations - 1, polymer.apply(rules), rules)

    input.partition(Polymer.regex.matches).use { case (polymerTemplateInput, insertionRulesInput) =>
      for {
        template <- polymerTemplateInput.runHead.someOrFailException.map(Polymer.apply)
        rules    <- insertionRulesInput.runCollect.map(InsertionRules.parse)
        polymer   = insertion(iterations, template, rules)
        sorted    = polymer.sortedOccurrences
        _        <- putStrLn(s"${sorted.last._2 - sorted.head._2}")
      } yield ()
    }
  }

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    mostCommonMinusLeastCommon(10, input)

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    mostCommonMinusLeastCommon(40, input)

}
