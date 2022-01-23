package AdventOfCode2015

object Day04:
  val md5 = java.security.MessageDigest.getInstance("MD5")

  def hash(string: String): Array[Byte] = md5.digest(string.getBytes)

  def search(salt: String, predicate: Array[Byte] => Boolean): Int =
    Iterator.from(1).indexWhere(index => predicate(hash(s"$salt$index"))) + 1

  def part1(input: String): Int = search(input, a => a(0) == 0 && a(1) == 0 && (a(2) & 0xF0) == 0)

  def part2(input: String): Int = search(input, a => a(0) == 0 && a(1) == 0 && a(2) == 0)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day04.txt").mkString.trim
    println(part1(data))
    println(part2(data))
