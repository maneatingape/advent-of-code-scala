package AdventOfCode2022

import scala.annotation.tailrec

object Day14:
  val start = Sand(500, 0)
  val moves = Seq((0, 1), (-1, 1), (1, 1))

  case class Sand(x: Int, y: Int):
    def move(dx: Int, dy: Int): Sand = Sand(x + dx, y + dy)

  def parse(input: Seq[String]): (Set[Sand], Int) =
    val cave = input.toSet.flatMap { line =>
      line.split("\\D+").map(_.toInt).sliding(4, 2).flatMap { case Array(x1, y1, x2, y2) =>
        for
          x <- x1.min(x2) to x1.max(x2)
          y <- y1.min(y2) to y1.max(y2)
        yield Sand(x, y)
      }
    }
    (cave, cave.map(_.y).max + 1)

  @tailrec
  def fall(cave: Set[Sand], floor: Int, sand: Sand): Sand =
    val next = moves.map(sand.move).filterNot(cave.contains)
    if sand.y == floor || next.isEmpty then sand else fall(cave, floor, next.head)

  @tailrec
  def simulate(cave: Set[Sand], floor: Int, predicate: Sand => Boolean): Int =
    val sand = fall(cave, floor, start)
    if predicate(sand) then cave.size else simulate(cave + sand, floor, predicate)

  def part1(input: Seq[String]): Int =
    val (rock, floor) = parse(input)
    simulate(rock, floor, _.y == floor) - rock.size

  def part2(input: Seq[String]): Int =
    val (rock, floor) = parse(input)
    simulate(rock, floor, _ == start) - rock.size + 1

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day14.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
