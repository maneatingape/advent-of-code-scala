package AdventOfCode2016

object Day15:
  case class Disc(index: Int, size: Int, offset: Int):
    def pass(time: Int) = (time + index + offset) % size == 0

  def parse(input: Seq[String]): Seq[Disc] = input.map { line =>
    val tokens = line.split("\\D+").tail.map(_.toInt)
    Disc(tokens(0), tokens(1), tokens(3))
  }

  def firstTime(discs: Seq[Disc]): Int = Iterator.from(0).filter(time => discs.forall(_.pass(time))).next()

  def part1(input: Seq[String]): Int = firstTime(parse(input))

  def part2(input: Seq[String]): Int =
    val discs = parse(input)
    val expanded = discs.appended(Disc(discs.size + 1, 11, 0))
    firstTime(expanded)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day15.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
