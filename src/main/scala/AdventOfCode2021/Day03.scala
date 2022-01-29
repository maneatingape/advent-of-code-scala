package AdventOfCode2021

object Day03:
  def part1(input: Seq[String]): Int = input
      .transpose.map(seq => if seq.count(_ == '1') > seq.length / 2 then Seq(1, 0) else Seq(0, 1))
      .transpose.map(seq => Integer.parseInt(seq.mkString, 2))
      .product

  def part2(input: Seq[String]): Int =
    def findRating(seq: Seq[String], index: Int, predicate: (Int, Int) => Boolean): Int =
      if seq.length == 1 then
        Integer.parseInt(seq.head, 2)
      else
        val count = seq.count(_.charAt(index) == '1')
        val threshold = (seq.length + 1) / 2
        val digit = if predicate(count, threshold) then '1' else '0'
        findRating(seq.filter(_.charAt(index) == digit), index + 1, predicate)

    val generatorRating = findRating(input, 0, _ >= _)
    val scrubberRating = findRating(input, 0, _ < _)
    generatorRating * scrubberRating
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day03.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
