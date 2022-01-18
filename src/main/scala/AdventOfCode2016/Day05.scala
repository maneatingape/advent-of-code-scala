package AdventOfCode2016

object Day05:
  def part1(input: String): String =
    val md5 = java.security.MessageDigest.getInstance("MD5")
    val buffer = collection.mutable.ArrayBuffer[String]()
    var index = 0

    while buffer.size < 8 do
      val hash = md5.digest(s"$input$index".getBytes)
      index += 1
      if hash(0) == 0 && hash(1) == 0 && (hash(2) & 0xF0) == 0 then
        buffer += (hash(2) & 0x0F).toHexString

    buffer.mkString
  end part1

  def part2(input: String): String =
    val md5 = java.security.MessageDigest.getInstance("MD5")
    val buffer = collection.mutable.Map[Int, String]()
    var index = 0

    while buffer.size < 8 do
      val hash = md5.digest(s"$input$index".getBytes)
      index += 1
      if hash(0) == 0 && hash(1) == 0 && (hash(2) & 0xF0) == 0 && hash(2) < 8 && !buffer.contains(hash(2)) then
        buffer += hash(2).toInt -> ((hash(3) >> 4) & 0x0F).toHexString

    (0 to 7).map(buffer).mkString
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day05.txt").mkString.trim
    println(part1(data))
    println(part2(data))
