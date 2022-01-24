package AdventOfCode2015

object Day15:
  def recipes(input: Seq[String]): Iterator[Seq[Int]] =
    val ingredients = input.map(_.split("[^-\\d]+").tail.map(_.toInt))
    def range(offset: Int) = Iterator.range(0, 101 - offset)
    for
      a <- range(0)
      b <- range(a)
      c <- range(a + b)
      d <- range(a + b + c)
      if a + b + c + d == 100
    yield Seq(a, b, c, d).zip(ingredients).map((k, i) => i.map(_ * k)).transpose.map(_.sum.max(0))

  def part1(input: Seq[String]): Int = recipes(input).map(_.init.product).max

  def part2(input: Seq[String]): Int = recipes(input).filter(_.last == 500).map(_.init.product).max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day15.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
