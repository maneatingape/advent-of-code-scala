package AdventOfCode2020

import scala.annotation.tailrec

object Day23:
  case class Cup(value: Int, var next: Cup = null)

  def parse(input: String): Seq[Int] = input.trim.map(_.asDigit)

  def play(seq: Seq[Int], max: Int): Cup =
    val cups = seq.scanRight(0 -> Cup(0)) { case (value, (nextValue, nextCup)) => value -> Cup(value, nextCup) }.toMap
    cups(seq.last).next = cups(seq.head)

    @tailrec
    def helper(cup: Cup, step: Int): Cup =
      if step == max then cups(1) else
        val first = cup.next
        val second = first.next
        val third = second.next
        val picked = Set(cup.value, first.value, second.value, third.value)
        val destination = Iterator.iterate(cup.value)(n => if n == 1 then seq.size else n - 1).dropWhile(picked.contains).next()

        cup.next = third.next
        third.next = cups(destination).next
        cups(destination).next = first

        helper(cup.next, step + 1)
      end if
    end helper

    helper(cups(seq.head), 0)
  end play

  def part1(input: String): String =
    val start = play(parse(input), 100)
    def helper(cup: Cup, seq: Seq[Int]): String =
      if cup == start then seq.mkString else helper(cup.next, seq.appended(cup.value))
    helper(start.next, Seq())

  def part2(input: String): Long =
    val seq = parse(input) ++ (10 to 1000000)
    val cup = play(seq, 10000000)
    val (first, second) = (cup.next.value, cup.next.next.value)
    first.toLong * second.toLong

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day23.txt").mkString
    println(part1(data))
    println(part2(data))
