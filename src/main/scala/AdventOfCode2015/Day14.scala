package AdventOfCode2015

object Day14:
  case class Reindeer(speed: Int, fly: Int, rest: Int):
    def distance(time: Int): Int =
      val total = fly + rest
      val complete = time / total
      val partial = (time % total).min(fly)
      speed * (complete * fly + partial)

  def parse(input: Seq[String]): Seq[Reindeer] = input.map { line =>
    val Array(speed, fly, rest) = line.split("\\D+").tail.map(_.toInt)
    Reindeer(speed, fly, rest)
  }

  def part1(input: Seq[String], time: Int): Int = parse(input).map(_.distance(time)).max

  def part2(input: Seq[String], time: Int): Int =
    val reindeer = parse(input)
    (1 to time)
      .map(time => reindeer.map(_.distance(time)))
      .map(state => state.map(d => if d == state.max then 1 else 0))
      .transpose.map(_.sum).max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day14.txt").getLines().toSeq
    println(part1(data, 2503))
    println(part2(data, 2503))
