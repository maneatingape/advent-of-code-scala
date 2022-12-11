package AdventOfCode2022

object Day11:
  case class Monkey(items: Seq[Long], operation: Long => Long, test: Int, yes: Int, no: Int, count: Long):
    def finish: Monkey = copy(items = Seq(), count = count + items.size)
    def accept(extra: Seq[Long]): Monkey = copy(items = items ++ extra)

  def parseNumbers(s: String): Seq[Int] = s.split("\\D+").filter(_.nonEmpty).map(_.toInt).toSeq

  def parseOperation(s: String): Long => Long = s.split(" ").takeRight(2) match
    case Array("*", "old") => o => o * o
    case Array("*", x) => _ * x.toLong
    case Array("+", x) => _ + x.toLong

  def parse(input: String): Seq[Monkey] =
    input.split("\n\n").map(_.split("\n")).toSeq.map { lines =>
      val items = parseNumbers(lines(1)).map(_.toLong)
      val operation = parseOperation(lines(2))
      val test = parseNumbers(lines(3)).last
      val yes = parseNumbers(lines(4)).last
      val no = parseNumbers(lines(5)).last
      Monkey(items, operation, test, yes, no, 0)
    }

  def step(monkeys: Seq[Monkey]): Seq[Monkey] =
    monkeys.indices.foldLeft(monkeys) { (monkeys, index) =>
      val Monkey(items, operation, test, yes, no, _) = monkeys(index)
      val (pass, fail) = items.map(operation).partition(_ % test == 0)
      monkeys
        .updated(index, monkeys(index).finish)
        .updated(yes, monkeys(yes).accept(pass))
        .updated(no, monkeys(no).accept(fail))
    }

  def play(monkeys: Seq[Monkey], rounds: Int): Long =
    Iterator.iterate(monkeys)(step).drop(rounds).next().map(_.count).sorted.takeRight(2).product

  def part1(input: String): Long =
    val monkeys = parse(input)
    val adjusted = monkeys.map(monkey => monkey.copy(operation = monkey.operation(_) / 3))
    play(adjusted, 20)

  def part2(input: String): Long =
    val monkeys = parse(input)
    val modulus = monkeys.map(_.test).product
    val adjusted = monkeys.map(monkey => monkey.copy(operation = monkey.operation(_) % modulus))
    play(adjusted, 10000)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day11.txt").mkString
    println(part1(data))
    println(part2(data))
