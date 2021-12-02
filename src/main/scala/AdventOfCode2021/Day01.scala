package AdventOfCode2021

object Day01 {
  val sample = Seq(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  val data = io.Source.fromResource("AdventOfCode2021/Day01.txt").getLines.map(_.toInt).toSeq

  def part1(input: Seq[Int]) = input.sliding(2).count { case Seq(x, y) => y > x }

  def part2(input: Seq[Int]) = part1(input.sliding(3).map(_.sum).toSeq)

  def main(args: Array[String]) = {
    println(part1(sample))
    println(part2(sample))
    println(part1(data))
    println(part2(data))
  }
}
