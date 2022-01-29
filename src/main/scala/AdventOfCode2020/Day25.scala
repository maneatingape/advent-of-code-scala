package AdventOfCode2020

object Day25:
  def part1(input: Seq[Int]): BigInt =
    val Seq(cardPub, doorPub) = input
    val doorLoopSize = Iterator.iterate(1)(n => (n * 7) % 20201227).indexWhere(_ == doorPub)
    BigInt(cardPub).modPow(doorLoopSize, 20201227)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day25.txt").getLines().map(_.toInt).toSeq
    println(part1(data))
