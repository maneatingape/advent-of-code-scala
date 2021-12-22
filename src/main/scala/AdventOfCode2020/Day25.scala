package AdventOfCode2020

object Day25:
  def part1(input: Seq[Int]): Long =
    val (cardPub, doorPub) = (input(0), input(1))

    def transform(subjectNumber: Long) = Iterator.iterate((0, 1L))((n, x) => (n + 1, (x * subjectNumber) % 20201227))
    def loopSize(pubKey: Int) = transform(7).dropWhile(_._2 != pubKey).next()._1

    val cardLoopSize = loopSize(cardPub)
    val doorLoopSize = loopSize(doorPub)
    transform(cardPub).drop(doorLoopSize).next()._2
  end part1

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day25.txt").getLines().map(_.toInt).toSeq
    println(part1(data))
