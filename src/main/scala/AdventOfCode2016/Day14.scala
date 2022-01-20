package AdventOfCode2016

object Day14:
  val md5 = java.security.MessageDigest.getInstance("MD5")
  val three = "(.)\\1{2}".r.unanchored
  val five = "(.)\\1{4}".r.unanchored

  def single(string: String): String = md5.digest(string.getBytes).map(_.formatted("%02x")).mkString

  def stretched(string: String): String = Iterator.iterate(string)(single).drop(2017).next()

  def generatePad(hash: Int => String): Int =
    def check(window: Seq[(String, Int)]): Boolean = window.head match
      case (three(t), _) => window.tail.exists {
        case (five(f), _) if f == t => true
        case _ => false
      }
      case _ => false

    Iterator.from(0).map(hash).zipWithIndex.sliding(1001).filter(check).drop(63).next().head._2
  end generatePad

  def part1(input: String): Int = generatePad(index => single(s"$input$index"))

  def part2(input: String): Int = generatePad(index => stretched(s"$input$index"))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day14.txt").mkString.trim
    println(part1(data))
    println(part2(data))
