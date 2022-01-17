package AdventOfCode2017

object Day21:
  val start = Seq(".#.", "..#", "###")

  type Pattern = Seq[String]
  type Rules = Map[Pattern, Pattern]

  def permutations(pattern: Pattern): Seq[Pattern] =
    val rotated = Iterator.iterate(pattern, 4)(_.reverse.transpose.map(_.mkString)).toSeq
    rotated ++ rotated.map(_.reverse)

  def parse(input: Seq[String]): Rules =
    input.flatMap { line =>
      val Array(first, second) = line.split(" => ")
      val pattern = first.split("/").toSeq
      val replacement = second.split("/").toSeq
      permutations(pattern).map(_ -> replacement)
    }
    .toMap

  def fractal(input: Seq[String], n: Int): Int =
    val rules = parse(input)
    def step(pattern: Pattern): Pattern =
      val size = if pattern.size % 2 == 0 then 2 else 3
      pattern
        .map(_.grouped(size).toSeq)
        .grouped(size).toSeq
        .flatMap(_.transpose.map(rules).transpose.map(_.mkString))

    val result = Iterator.iterate(start)(step).drop(n).next()
    result.map(_.count(_ == '#')).sum
  end fractal

  def part1(input: Seq[String]): Int = fractal(input, 5)

  def part2(input: Seq[String]): Int = fractal(input, 18)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day21.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
