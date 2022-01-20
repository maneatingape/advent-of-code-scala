package AdventOfCode2016

object Day16:
  def dragon(start: String, target: Int): String =
    def step(s: String) = s.appended('0').appendedAll(s.reverse.map(b => if b == '1' then '0' else '1'))
    Iterator.iterate(start)(step).dropWhile(_.length < target).next().take(target)

  def checksum(start: String): String =
    val groupSize = Integer.lowestOneBit(start.length)
    def step(s: String) = s.grouped(groupSize).map(g => if g.count(_ == '1') % 2 == 0 then '1' else '0').mkString
    Iterator.iterate(start)(step).dropWhile(_.length % 2 == 0).next()

  def part1(input: String): String = checksum(dragon(input, 272))

  def part2(input: String): String = checksum(dragon(input, 35651584))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day16.txt").mkString.trim
    println(part1(data))
    println(part2(data))
