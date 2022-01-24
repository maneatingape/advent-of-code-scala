package AdventOfCode2015

object Day11:
  def increment(password: String): String =
    def helper(string: String, index: Int): String =
      if string(index) == 'z' then helper(string.updated(index, 'a'), index - 1)
      else string.updated(index, (string(index) + 1).toChar)
    helper(password, password.size - 1)

  def requirement(password: String): Boolean =
    password.sliding(3).exists(w => w(1) - w(0) == 1 && w(2) - w(1) == 1)
    && !password.exists(c => c == 'i' || c == 'o' || c == 'l')
    && password.sliding(2).filter(w => w(0) == w(1)).toSet.size >= 2

  def next(input: String): String = Iterator.iterate(input)(increment).drop(1).filter(requirement).next()

  def part1(input: String): String = next(input)

  def part2(input: String): String = next(next(input))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day11.txt").mkString.trim
    println(part1(data))
    println(part2(data))
