package AdventOfCode2018

import scala.annotation.tailrec

object Day07:
  def parse(input: Seq[String]): Map[String, Set[String]] =
    val regex = "Step (\\w) must be finished before step (\\w) can begin.".r
    input.foldLeft(Map.empty[String, Set[String]].withDefaultValue(Set())) { case (steps, regex(first, second)) =>
      steps.updated(first, steps(first)).updated(second, steps(second) + first)
    }

  def part1(input: Seq[String]): String =
    def helper(remaining: Map[String, Set[String]], done: Seq[String]): String =
      if remaining.isEmpty then done.mkString else
        val next = remaining.collect { case (k, v) if v.isEmpty => k }.min
        val nextRemaining = remaining.removed(next).view.mapValues(_ - next).toMap
        helper(nextRemaining, done.appended(next))

    helper(parse(input), Seq())
  end part1

  def part2(input: Seq[String], workers: Int, duration: Int): Int =
    case class Task(step: String, remaining: Int):
      def decrement: Task = copy(remaining = remaining - 1)

    @tailrec
    def helper(remaining: Map[String, Set[String]], doing: Seq[Task], seconds: Int): Int =
      if remaining.isEmpty && doing.isEmpty then
        seconds
      else if doing.size < workers && remaining.values.exists(_.isEmpty) then
        val next = remaining.collect { case (k, v) if v.isEmpty => k }.min
        val task = Task(next, duration + next.head - 'A')
        helper(remaining.removed(next), doing.appended(task), seconds)
      else
        val (done, todo) = doing.partition(_.remaining == 0)
        val nextRemaining = remaining.view.mapValues(_ -- done.map(_.step)).toMap
        val nextDoing = todo.map(_.decrement)
        helper(nextRemaining, nextDoing, seconds + 1)

    helper(parse(input), Seq(), 0)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day07.txt").getLines().toSeq
    println(part1(data))
    println(part2(data, 5, 60))
