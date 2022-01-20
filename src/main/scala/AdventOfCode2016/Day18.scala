package AdventOfCode2016

object Day18:
  def safe(input: String, count: Int): Int =
    val width = input.length
    val start = BigInt(input.map(c => if c == '^' then '1' else '0'), 2)
    def step(n: BigInt) = (n << 1).clearBit(width) ^ (n >> 1)
    Iterator.iterate(start)(step).map(width - _.bitCount).take(count).sum

  def part1(input: String): Int = safe(input, 40)

  def part2(input: String): Int = safe(input, 400000)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day18.txt").mkString.trim
    println(part1(data))
    println(part2(data))
