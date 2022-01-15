package AdventOfCode2017

object Day16:
  val start = "abcdefghijklmnop"
  val spin = "s(\\d+)".r
  val exchange = "x(\\d+)/(\\d+)".r
  val partner = "p(\\w)/(\\w)".r

  def step(moves: Seq[String], programs: String): String = moves.foldLeft(programs) { (programs, move) =>
    move match
      case spin(a) =>
        val x = a.toInt
        programs.takeRight(x) ++ programs.dropRight(x)
      case exchange(a, b) =>
        val x = a.toInt
        val y = b.toInt
        programs.updated(x, programs(y)).updated(y, programs(x))
      case partner(a, b) =>
        val x = programs.indexOf(a)
        val y = programs.indexOf(b)
        programs.updated(x, programs(y)).updated(y, programs(x))
  }

  def part1(input: Seq[String]): String = step(input, start)

  def part2(input: Seq[String]): String =
    def dances = Iterator.iterate(start)(step(input, _))
    val period = dances.indexOf(start, 1)
    dances.drop(1000000000 % period).next()

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day16.txt").mkString.trim.split(",")
    println(part1(data))
    println(part2(data))
