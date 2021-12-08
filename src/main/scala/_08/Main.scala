package _08

import common.Runner
import zio._
import zio.console._
import zio.stream._

object Main extends Runner {

  sealed trait Segment
  case object a extends Segment
  case object b extends Segment
  case object c extends Segment
  case object d extends Segment
  case object e extends Segment
  case object f extends Segment
  case object g extends Segment

  object Segment {

    def apply(char: Char): Option[Segment] =
      char match {
        case 'a' => Some(a)
        case 'b' => Some(b)
        case 'c' => Some(c)
        case 'd' => Some(d)
        case 'e' => Some(e)
        case 'f' => Some(f)
        case 'g' => Some(g)
        case _   => None
      }

  }

  sealed trait Digit {

    def segments: Set[Segment]

    def same(other: Digit): Boolean = segments == other.segments

  }

  sealed trait ResolvedDigit extends Digit { self =>

    override def toString: String =
      self match {
        case Zero(_)  => "0"
        case One(_)   => "1"
        case Two(_)   => "2"
        case Three(_) => "3"
        case Four(_)  => "4"
        case Five(_)  => "5"
        case Six(_)   => "6"
        case Seven(_) => "7"
        case Eight(_) => "8"
        case Nine(_)  => "9"
      }

  }
  case class Zero(segments: Set[Segment])  extends ResolvedDigit
  case class One(segments: Set[Segment])   extends ResolvedDigit
  case class Two(segments: Set[Segment])   extends ResolvedDigit
  case class Three(segments: Set[Segment]) extends ResolvedDigit
  case class Four(segments: Set[Segment])  extends ResolvedDigit
  case class Five(segments: Set[Segment])  extends ResolvedDigit
  case class Six(segments: Set[Segment])   extends ResolvedDigit
  case class Seven(segments: Set[Segment]) extends ResolvedDigit
  case class Eight(segments: Set[Segment]) extends ResolvedDigit
  case class Nine(segments: Set[Segment])  extends ResolvedDigit

  case class UnresolvedDigit(segments: Set[Segment]) extends Digit {

    def asZero: Zero = Zero(segments)

    def asOne: One = One(segments)

    def asTwo: Two = Two(segments)

    def asThree: Three = Three(segments)

    def asFour: Four = Four(segments)

    def asFive: Five = Five(segments)

    def asSix: Six = Six(segments)

    def asSeven: Seven = Seven(segments)

    def asEight: Eight = Eight(segments)

    def asNine: Nine = Nine(segments)

  }

  object UnresolvedDigit {

    def apply(input: String): UnresolvedDigit =
      UnresolvedDigit(input.flatMap(Segment.apply).toSet)

  }

  case class SignalPatterns(digits: Set[UnresolvedDigit]) extends AnyVal

  case class DigitOutput(digits: Chunk[UnresolvedDigit]) extends AnyVal

  case class Line(patterns: SignalPatterns, output: DigitOutput)

  object Line {

    def parse(input: String): Line = {
      val elems    = input.split(' ')
      val patterns = SignalPatterns(elems.takeWhile(_ != "|").map(UnresolvedDigit.apply).toSet)
      val output   = DigitOutput(Chunk.fromIterable(elems.dropWhile(_ != "|").tail.map(UnresolvedDigit.apply)))
      Line(patterns, output)
    }

  }

  def isUnique(digit: UnresolvedDigit): Boolean = isOne(digit) || isFour(digit) || isSeven(digit) || isEight(digit)

  def isZero(six: Six, nine: Nine)(digit: UnresolvedDigit): Boolean =
    digit.segments.sizeIs == 6 && !digit.same(six) && !digit.same(nine)

  def isOne(digit: UnresolvedDigit): Boolean = digit.segments.sizeIs == 2

  def isTwo(three: Three, five: Five)(digit: UnresolvedDigit): Boolean =
    digit.segments.sizeIs == 5 && !digit.same(three) && !digit.same(five)

  def isThree(nine: Nine)(digit: UnresolvedDigit): Boolean =
    digit.segments.sizeIs == 5 && ((nine.segments &~ digit.segments) ++ digit.segments) == nine.segments

  def isFour(digit: UnresolvedDigit): Boolean = digit.segments.sizeIs == 4

  def isFive(one: One, nine: Nine)(digit: UnresolvedDigit): Boolean =
    digit.segments.sizeIs == 5 && (digit.segments &~ (nine.segments &~ one.segments)).sizeIs == 1

  def isSix(one: One, eight: Eight)(digit: UnresolvedDigit): Boolean =
    digit.segments.sizeIs == 6 && (digit.segments &~ (eight.segments &~ one.segments)).sizeIs == 1

  def isSeven(digit: UnresolvedDigit): Boolean = digit.segments.sizeIs == 3

  def isEight(digit: UnresolvedDigit): Boolean = digit.segments.sizeIs == 7

  def isNine(four: Four, seven: Seven)(digit: UnresolvedDigit): Boolean =
    digit.segments.sizeIs == 6 && (digit.segments &~ four.segments &~ seven.segments).sizeIs == 1

  def resolve(patterns: SignalPatterns, output: DigitOutput): Int = {

    def zero(unresolved: Set[UnresolvedDigit], six: Six, nine: Nine): (Zero, Set[UnresolvedDigit]) = {
      val (zeroes, remaining) = unresolved.partition(isZero(six, nine))
      (zeroes.headOption.map(_.asZero).getOrElse(sys.error("Zero not found in unresolved digits")), remaining)
    }

    def one(unresolved: Set[UnresolvedDigit]): (One, Set[UnresolvedDigit]) = {
      val (ones, remaining) = unresolved.partition(isOne)
      (ones.headOption.map(_.asOne).getOrElse(sys.error("One not found in unresolved digits")), remaining)
    }

    def two(unresolved: Set[UnresolvedDigit], three: Three, five: Five): (Two, Set[UnresolvedDigit]) = {
      val (twos, remaining) = unresolved.partition(isTwo(three, five))
      (twos.headOption.map(_.asTwo).getOrElse(sys.error("Two not found in unresolved digits")), remaining)
    }

    def three(unresolved: Set[UnresolvedDigit], nine: Nine): (Three, Set[UnresolvedDigit]) = {
      val (threes, remaining) = unresolved.partition(isThree(nine))
      (threes.headOption.map(_.asThree).getOrElse(sys.error("Three not found in unresolved digits")), remaining)
    }

    def four(unresolved: Set[UnresolvedDigit]): (Four, Set[UnresolvedDigit]) = {
      val (fours, remaining) = unresolved.partition(isFour)
      (fours.headOption.map(_.asFour).getOrElse(sys.error("Four not found in unresolved digits")), remaining)
    }

    def five(unresolved: Set[UnresolvedDigit], one: One, nine: Nine): (Five, Set[UnresolvedDigit]) = {
      val (fives, remaining) = unresolved.partition(isFive(one, nine))
      (fives.headOption.map(_.asFive).getOrElse(sys.error("Five not found in unresolved digits")), remaining)
    }

    def six(unresolved: Set[UnresolvedDigit], one: One, eight: Eight): (Six, Set[UnresolvedDigit]) = {
      val (sixes, remaining) = unresolved.partition(isSix(one, eight))
      (sixes.headOption.map(_.asSix).getOrElse(sys.error("Six not found in unresolved digits")), remaining)
    }

    def seven(unresolved: Set[UnresolvedDigit]): (Seven, Set[UnresolvedDigit]) = {
      val (sevens, remaining) = unresolved.partition(isSeven)
      (sevens.headOption.map(_.asSeven).getOrElse(sys.error("Seven not found in unresolved digits")), remaining)
    }

    def eight(unresolved: Set[UnresolvedDigit]): (Eight, Set[UnresolvedDigit]) = {
      val (eights, remaining) = unresolved.partition(isEight)
      (eights.headOption.map(_.asEight).getOrElse(sys.error("Eight not found in unresolved digits")), remaining)
    }

    def nine(unresolved: Set[UnresolvedDigit], four: Four, seven: Seven): (Nine, Set[UnresolvedDigit]) = {
      val (nines, remaining) = unresolved.partition(isNine(four, seven))
      (nines.headOption.map(_.asNine).getOrElse(sys.error("Nine not found in unresolved digits")), remaining)
    }

    val (_one, afterOne)     = one(patterns.digits)
    val (_four, afterFour)   = four(afterOne)
    val (_seven, afterSeven) = seven(afterFour)
    val (_eight, afterEight) = eight(afterSeven)
    val (_nine, afterNine)   = nine(afterEight, _four, _seven)
    val (_five, afterFive)   = five(afterNine, _one, _nine)
    val (_six, afterSix)     = six(afterFive, _one, _eight)
    val (_three, afterThree) = three(afterSix, _nine)
    val (_two, afterTwo)     = two(afterThree, _three, _five)
    val (_zero, _)           = zero(afterTwo, _six, _nine)

    val numbers = Set(_zero, _one, _two, _three, _four, _five, _six, _seven, _eight, _nine)

    output.digits.flatMap(digit => numbers.find(_.same(digit))).foldLeft("")(_ + _.toString).toInt
  }

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      count <- input
                 .mapConcat(line => Line.parse(line).output.digits)
                 .filter(isUnique)
                 .runCount
      _     <- putStrLn(count.toString)
    } yield ()

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] =
    for {
      sum <- input
               .map(Line.parse)
               .map(line => resolve(line.patterns, line.output))
               .runSum
      _   <- putStrLn(sum.toString)
    } yield ()

}
