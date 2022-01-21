package AdventOfCode2019

object Day22:
  case class Technique(a: BigInt, c: BigInt, m: BigInt):
    def shuffle(index: Long): Long = mod(a * index + c).toLong

    def merge(other: Technique): Technique =
      val nextA = mod(a * other.a)
      val nextC = mod(c * other.a + other.c)
      Technique(nextA, nextC, m)

    def inverse: Technique =
      val nextA = a.modInverse(m)
      val nextC = mod(nextA * -c)
      Technique(nextA, nextC, m)

    private def mod(n: BigInt) = n % m
  end Technique

  def parse(input: Seq[String], size: Long): Technique = input.map(_.split(" "))
    .map {
      case Array(_, "into", _, _) => Technique(size - 1, size - 1, size)
      case Array(_, "with" ,_, n) => Technique(n.toLong, 0, size)
      case Array("cut", n) => Technique(1, size - n.toLong, size)
    }
    .reduce(_ merge _)

  def part1(input: Seq[String]): Long = parse(input, 10007).shuffle(2019)

  def part2(input: Seq[String]): Long =
    def helper(remaining: Long, result: Long, technique: Technique): Long = if remaining == 0 then result else
      val nextResult = if remaining % 2 == 0 then result else technique.shuffle(result)
      helper(remaining / 2, nextResult, technique.merge(technique))

    helper(101741582076661L, 2020, parse(input, 119315717514047L).inverse)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day22.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
