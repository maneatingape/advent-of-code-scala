package AdventOfCode2015

object Day25:
  def parse(input: String): (Int, Int) =
    val Array(row, column) = input.split("\\D+").tail.map(_.toInt)
    (row, column)

  def diagonalIndex(row: Int, column: Int): Int =
    val n = row + column - 2
    val sum = (n * (n + 1)) / 2
    sum + column - 1

  def code(n: Int): Int = ((20151125 * BigInt(252533).modPow(n, 33554393)) % 33554393).toInt

  def part1(input: String): Int = code(diagonalIndex.tupled(parse(input)))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day25.txt").mkString.trim
    println(part1(data))
