package AdventOfCode2019

import Day09.IntCode

object Day21:
  val walk = Seq("OR A J", "AND B J", "AND C J", "NOT J J", "AND D J")
  val run = Seq("OR E T", "OR H T", "AND T J")

  def survey(memory: Seq[Long], script: Seq[String]): Long =
    val input = script.map(_ + "\n").mkString.map(_.toLong)
    IntCode(memory).withInput(input*).allOutput.last

  def part1(memory: Seq[Long]): Long = survey(memory, walk.appended("WALK"))

  def part2(memory: Seq[Long]): Long = survey(memory, walk ++ run.appended("RUN"))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day21.txt").mkString.trim.split(",").map(_.toLong).toSeq
    println(part1(data))
    println(part2(data))
