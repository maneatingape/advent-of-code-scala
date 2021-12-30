package AdventOfCode2019

object Day16:
  def part1(input: String): String =
    def phase(input: Seq[Int]): Seq[Int] = (0 until input.size).map { n =>
      val raw = input.zipWithIndex.map { (next, index) =>
        ((index + 1) / (n + 1)) % 4 match
          case 0 | 2 => 0
          case 1 => next
          case 3 => -next
      }
      (raw.sum % 10).abs
    }

    val initial = input.map(_.asDigit)
    Iterator.iterate(initial)(phase).drop(100).next().take(8).mkString
  end part1

  def part2(input: String): String =
    def phase(input: Seq[Int]): Seq[Int] = input.scanRight(0)(_ + _).map(_ % 10)

    val index = input.take(7).toInt
    val initial = (input * 10000).drop(index).map(_.asDigit)
    Iterator.iterate(initial)(phase).drop(100).next().take(8).mkString
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day16.txt").mkString.trim
    println(part1(data))
    println(part2(data))
