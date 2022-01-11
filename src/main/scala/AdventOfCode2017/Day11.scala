package AdventOfCode2017

object Day11:
  case class Hex(x: Int, y: Int):
    private def even: Boolean = x % 2 == 0
    def step(next: String): Hex = next match
      case "n" => Hex(x, y - 1)
      case "s" => Hex(x, y + 1)
      case "ne" => if even then Hex(x + 1, y) else Hex(x + 1, y - 1)
      case "nw" => if even then Hex(x - 1, y) else Hex(x - 1, y - 1)
      case "se" => if even then Hex(x + 1, y + 1) else Hex(x + 1, y)
      case "sw" => if even then Hex(x - 1, y + 1) else Hex(x - 1, y)
    def distance: Int =
      val offset = if y > 0 then (x.abs + 1) / 2 else (x.abs / 2)
      x.abs + (y.abs - offset).max(0)

  def walk(input: String): Seq[Int] = input.split(",").scanLeft(Hex(0, 0))(_.step(_)).map(_.distance)

  def part1(input: String): Int = walk(input).last

  def part2(input: String): Int = walk(input).max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day11.txt").mkString.trim
    println(part1(data))
    println(part2(data))
