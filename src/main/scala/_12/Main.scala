package _12

import common.Runner
import zio._
import zio.console._
import zio.stream._

import scala.util.matching.Regex

object Main extends Runner {

  sealed trait Cave
  case object Start              extends Cave
  case class Small(name: String) extends Cave
  case class Big(name: String)   extends Cave
  case object End                extends Cave

  object Cave {

    def parse(name: String): Cave =
      name match {
        case "start"                          => Start
        case "end"                            => End
        case name if name.toLowerCase == name => Small(name)
        case name if name.toUpperCase == name => Big(name)
        case _                                => throw new IllegalArgumentException(s"Invalid cave name: $name")
      }

  }

  case class Path(caves: Seq[Cave]) extends AnyVal

  case class Connections(conns: Map[Cave, Set[Cave]]) {

    def paths(canVisit: (Map[Cave, Int], Cave) => Boolean): Set[Path] = {

      def walk(visited: List[Cave], accums: Map[Cave, Int]): Set[Path] =
        visited.head match {
          case End     => Set(Path(visited.reverse))
          case Start   =>
            conns
              .getOrElse(Start, Set.empty)
              .flatMap(next => walk(next :: visited, Map(next -> 1)))
          case current =>
            conns
              .getOrElse(current, Set.empty)
              .filter(next => canVisit(accums, next))
              .flatMap(next => walk(next :: visited, accums.updatedWith(next)(times => Some(times.fold(1)(_ + 1)))))
        }

      walk(List(Start), Map.empty)
    }

  }

  object Connections {

    private val regex: Regex = """([a-zA-Z]+)-([a-zA-Z]+)""".r

    def parse(lines: Chunk[String]): Connections =
      Connections(lines.foldLeft(Map.empty[Cave, Set[Cave]]) { (map, line) =>
        line match {
          case regex(a, b) =>
            (Cave.parse(a), Cave.parse(b)) match {
              case (s @ Start, to) => map.updatedWith(s)(v => Some(v.fold(Set(to))(_ + to)))
              case (to, s @ Start) => map.updatedWith(s)(v => Some(v.fold(Set(to))(_ + to)))
              case (from, e @ End) => map.updatedWith(from)(v => Some(v.fold(Set(e))(_ + e)))
              case (e @ End, from) => map.updatedWith(from)(v => Some(v.fold(Set(e))(_ + e)))
              case (a, b)          =>
                map
                  .updatedWith(a)(v => Some(v.fold(Set(b))(_ + b)))
                  .updatedWith(b)(v => Some(v.fold(Set(a))(_ + a)))
            }
          case _           => throw new IllegalArgumentException(s"Invalid line: $line")
        }
      })

  }

  override def partOne(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] = {

    def canVisit(visited: Map[Cave, Int], next: Cave): Boolean =
      next match {
        case Start    => false
        case Small(_) => !visited.contains(next)
        case Big(_)   => true
        case End      => true
      }

    for {
      connections <- input.runCollect.map(Connections.parse)
      paths        = connections.paths(canVisit)
      _           <- putStrLn(paths.size.toString)
    } yield ()
  }

  override def partTwo(input: ZStream[Any, Throwable, String]): RIO[ZEnv, Unit] = {

    def canVisit(visited: Map[Cave, Int], next: Cave): Boolean =
      next match {
        case Start        => false
        case s @ Small(_) =>
          !visited.contains(s) || visited.forall {
            case (Small(_), times) => times == 1
            case _                 => true
          }
        case Big(_)       => true
        case End          => true
      }

    for {
      connections <- input.runCollect.map(Connections.parse)
      paths        = connections.paths(canVisit)
      _           <- putStrLn(paths.size.toString)
    } yield ()
  }

}
