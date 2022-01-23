package AdventOfCode2015

object Day06:
  val on = "turn on.*".r
  val off = "turn off.*".r
  val toggle = "toggle.*".r

  def part1(input: Seq[String]): Int =
    val grid = Array.fill(1000)(Array.fill(1000)(0))
    input.foreach { line =>
      val func: Int => Int = line match
        case on() => p => 1
        case off() => p => 0
        case toggle() => p => if p == 1 then 0 else 1

      val Array(x1, y1, x2, y2) = line.split("\\D+").tail.map(_.toInt)
      for x <- x1 to x2; y <- y1 to y2 do grid(y)(x) = func(grid(y)(x))
    }
    grid.map(_.sum).sum

  def part2(input: Seq[String]): Int =
    val grid = Array.fill(1000)(Array.fill(1000)(0))
    input.foreach { line =>
      val func: Int => Int = line match
        case on() => p => p + 1
        case off() => p => (p - 1).max(0)
        case toggle() => p => p + 2

      val Array(x1, y1, x2, y2) = line.split("\\D+").tail.map(_.toInt)
      for x <- x1 to x2; y <- y1 to y2 do grid(y)(x) = func(grid(y)(x))
    }
    grid.map(_.sum).sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day06.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
