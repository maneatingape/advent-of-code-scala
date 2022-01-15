package AdventOfCode2017

object Day15:
  def parse(input: Seq[String]): (Iterator[Long], Iterator[Long]) =
    val Seq(a, b) = input.map(_.filter(_.isDigit).toLong)
    (generator(a, 16807), generator(b, 48271))

  def generator(start: Long, factor: Long): Iterator[Long] =
    Iterator.iterate(start)(n => (n * factor) % 2147483647).drop(1)

  def judge(a: Long, b: Long): Boolean = (a & 0xFFFF) == (b & 0xFFFF)

  def part1(input: Seq[String]): Int =
    val (a, b) = parse(input)
    a.zip(b).take(40000000).count(judge)

  def part2(input: Seq[String]): Int =
    val (a, b) = parse(input)
    a.filter(_ % 4 == 0).zip(b.filter(_ % 8 == 0)).take(5000000).count(judge)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day15.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
