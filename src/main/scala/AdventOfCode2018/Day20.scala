package AdventOfCode2018

import scala.annotation.tailrec

object Day20:
  case class Point(x: Int, y: Int):
    def move(direction: Char): Point = direction match
      case 'N' => Point(x, y - 1)
      case 'S' => Point(x, y + 1)
      case 'E' => Point(x - 1, y)
      case 'W' => Point(x + 1, y)

  @tailrec
  def walk(remaining: String, distance: Map[Point, Int] = Map(), paths: Set[Point] = Set(), stack: List[Set[Point]] = Nil): Seq[Int] = remaining.head match
    case '^' => walk(remaining.tail, Map(Point(0, 0) -> 0), Set(Point(0, 0)), Nil)
    case '$' => distance.values.toSeq
    case '(' => walk(remaining.tail, distance, paths, Set() :: paths :: stack)
    case '|' =>
      val accumulated :: previous :: tail = stack
      walk(remaining.tail, distance, previous, (accumulated ++ paths) :: previous :: tail)
    case ')' =>
      val accumulated :: previous :: tail = stack
      walk(remaining.tail, distance, accumulated ++ paths, tail)
    case direction =>
      val (nextDistance, nextPaths) = paths.foldLeft((distance, Set.empty[Point])) { case ((distance, paths), point) =>
        val next = point.move(direction)
        if distance.contains(next) && distance(next) <= distance(point) + 1 then (distance, paths + next)
        else (distance.updated(next, distance(point) + 1), paths + next)
      }
      walk(remaining.tail, nextDistance, nextPaths, stack)

  def part1(input: String): Int = walk(input).max

  def part2(input: String, threshold: Int): Int = walk(input).count(_ >= threshold)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day20.txt").mkString.trim
    println(part1(data))
    println(part2(data, 1000))
