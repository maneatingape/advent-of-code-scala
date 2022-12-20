package AdventOfCode2022

object Day20:
  def decrypt(input: Seq[Int], key: Long, rounds: Int): Long =
    val mixed = collection.mutable.ArrayBuffer.from(input.map(_ * key).zipWithIndex)
    for _ <- 1 to rounds do
      for index <- input.indices do
        val from = mixed.indexWhere(_._2 == index)
        val pair @ (number, _) = mixed.remove(from)
        val remainder = (number % mixed.size).toInt
        val to = (from + remainder + mixed.size) % mixed.size
        mixed.insert(to, pair)
      end for
    end for
    val start = mixed.indexWhere(_._1 == 0)
    (1 to 3).map(offset => mixed((start + 1000 * offset) % mixed.size)._1).sum

  def part1(input: Seq[Int]): Long = decrypt(input, 1L, 1)

  def part2(input: Seq[Int]): Long = decrypt(input, 811589153L, 10)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day20.txt").getLines().map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
