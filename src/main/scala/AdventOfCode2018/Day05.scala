package AdventOfCode2018

import scala.annotation.tailrec

object Day05:
  def react(l: Char, r: Char): Boolean = (l.toLower == r.toLower) && (l.isLower ^ r.isLower)

  def shorten(input: String): Int =
    @tailrec
    def helper(left: List[Char], right: List[Char]): Int = (left, right) match
      case (left, Nil) => left.size
      case (left :: ls, right :: rs) if react(left, right) => helper(ls, rs)
      case (left, right :: rs) => helper(right :: left, rs)

    helper(Nil, input.toList)
  end shorten

  def part1(input: String): Int = shorten(input)

  def part2(input: String): Int =
    val variations = for unitType <- 'a' to 'z' yield input.filter(_.toLower != unitType)
    variations.map(shorten).min

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day05.txt").mkString.trim
    println(part1(data))
    println(part2(data))
