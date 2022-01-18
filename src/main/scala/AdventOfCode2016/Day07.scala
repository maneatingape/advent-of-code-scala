package AdventOfCode2016

object Day07:
  def parse(input: Seq[String]): Seq[(Set[String], Set[String])] = input.map { line =>
    val blocks = line.split("[\\[\\]]")
    val (accept, reject) = blocks.zipWithIndex.partition((_, index) => index % 2 == 0)
    accept.map(_._1).toSet -> reject.map(_._1).toSet
  }

  def abba(block: String): Boolean = block.toSeq.sliding(4).exists {
    case Seq(a, b, c, d) => a != b && a == d && b == c
  }

  def aba(block: String): Seq[String] = block.toSeq.sliding(3).toSeq.collect {
    case Seq(a, b, c) if a != b && a == c => s"$a$b$a"
  }

  def bab(block: String): Seq[String] = block.toSeq.sliding(3).toSeq.collect {
    case Seq(a, b, c) if a != b && a == c => s"$b$a$b"
  }

  def part1(input: Seq[String]): Int = parse(input).count { (accept, reject) =>
    accept.exists(abba) && !reject.exists(abba)
  }

  def part2(input: Seq[String]): Int = parse(input).count { (accept, reject) =>
    accept.flatMap(aba).intersect(reject.flatMap(bab)).size > 0
  }

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day07.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
