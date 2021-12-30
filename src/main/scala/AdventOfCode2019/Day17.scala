package AdventOfCode2019

import Day09.IntCode

object Day17:
  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)

  def part1(memory: Seq[Long]): Int =
    val input = IntCode(memory).allOutput.map(_.toChar).mkString.split("\n")
    val points = for y <- 0 until input.size; x <- 0 until input.head.size if input(y)(x) == '#' yield Point(x, y)
    val neighbours = Seq(Point(0, -1), Point(0, 1), Point(-1, 0), Point(1, 0))
    points.foldLeft(0) { (total, next) =>
      if neighbours.map(next + _).forall(points.contains) then total + (next.x * next.y) else total
    }

  def part2(memory: Seq[Long]): Long =
    val mainRoutine = "A,B,B,A,C,A,C,A,C,B"
    val functionA = "L,6,R,12,R,8"
    val functionB = "R,8,R,12,L,12"
    val functionC = "R,12,L,12,L,4,L,4"
    val videoFeed = "n"
    val input = Seq(mainRoutine, functionA, functionB, functionC, videoFeed).map(_ + "\n").flatten.map(_.toLong)
    IntCode(memory.updated(0, 2)).withInput(input*).allOutput.last

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day17.txt").mkString.trim.split(",").map(_.toLong).toSeq
    println(part1(data))
    println(part2(data))
