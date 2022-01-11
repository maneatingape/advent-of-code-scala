package AdventOfCode2017

object Day11:
  case class Hex(q: Int, r: Int):
    def manhattan: Int = q.abs.max(r.abs)
    def step(next: String): Hex = next match
      case "n" => Hex(q + 1, r - 1)
      case "s" => Hex(q - 1, r + 1)
      case "ne" => Hex(q + 1, r)
      case "sw" => Hex(q - 1, r)
      case "nw" => Hex(q, r - 1)
      case "se" => Hex(q, r + 1)

  def walk(input: String): Seq[Int] = input.split(",").scanLeft(Hex(0, 0))(_.step(_)).map(_.manhattan)

  def part1(input: String): Int = walk(input).last

  def part2(input: String): Int = walk(input).max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day11.txt").mkString.trim
    println(part1(data))
    println(part2(data))
