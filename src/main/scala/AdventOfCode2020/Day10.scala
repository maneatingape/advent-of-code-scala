package AdventOfCode2020

object Day10:
  def part1(input: Seq[Int]): Long =
    val steps = (Seq(0, input.max + 3) ++ input).sorted.sliding(2).map(w => w(1) - w(0)).toSeq.groupBy(identity)
    steps(1).size * steps(3).size

  def part2(input: Seq[Int]): Long =
    val arrangements = input.sorted.foldLeft(Map(0 -> 1L).withDefaultValue(0L)) { (map, next) =>
      map.updated(next, map(next - 1) + map(next - 2) + map(next - 3))
    }
    arrangements(input.max)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day10.txt").getLines().map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
