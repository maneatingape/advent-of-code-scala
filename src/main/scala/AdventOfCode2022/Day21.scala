package AdventOfCode2022

object Day21:
  def parse(input: Seq[String], part2: Boolean): collection.mutable.Map[String, () => Long] =
    val monkeys = collection.mutable.Map[String, () => Long]()
    input.foreach { line =>
      val Array(name, rest: _*) = line.split("[: ]+"): @unchecked
      monkeys(name) = rest match
        case Seq(number) => () => number.toLong
        case Seq(left, operation, right) => operation match
          case _ if name == "root" && part2 => () => (monkeys(left)() - monkeys(right)()).abs
          case "+" => () => monkeys(left)() + monkeys(right)()
          case "-" => () => monkeys(left)() - monkeys(right)()
          case "*" => () => monkeys(left)() * monkeys(right)()
          case "/" => () => monkeys(left)() / monkeys(right)()
    }
    monkeys

  def part1(input: Seq[String]): Long = parse(input, false)("root")()

  def part2(input: Seq[String]): Long =
    val monkeys = parse(input, true)

    def check(n: Long): Long =
      monkeys("humn") = () => n
      monkeys("root")()

    def helper(prev: Long, n: Long, step: Long): Long =
      val next = n + step
      val result = check(next)
      if result == 0 then next
      else if result < prev then helper(result, next, step)
      else helper(result, next, step / -2)

    helper(check(0), 0, 1 << 60)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day21.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
