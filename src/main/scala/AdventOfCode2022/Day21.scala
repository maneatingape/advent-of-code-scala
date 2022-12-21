package AdventOfCode2022

object Day21:
  def parse(input: Seq[String]): collection.mutable.Map[String, () => Long] =
    val monkeys = collection.mutable.Map[String, () => Long]()
    def compute(name: String): Long = monkeys(name)()

    input.foreach { line =>
      val Array(name, rest: _*) = line.split("[: ]+"): @unchecked
      monkeys(name) = rest match
        case Seq(number) => () => number.toLong
        case Seq(left, op, right) => op match
          case "+" => () => compute(left) + compute(right)
          case "-" => () => compute(left) - compute(right)
          case "*" => () => compute(left) * compute(right)
          case "/" => () => compute(left) / compute(right)
    }

    monkeys
  end parse

  def part1(input: Seq[String]): Long = parse(input)("root")()

  def part2(input: Seq[String]): Long =
    val monkeys = parse(input)
    var start = 3_000_000_000_000L
    var middle = 0L
    var end = 4_000_000_000_000L

    while (start < end) do {
      val middle = (start + end) / 2

      monkeys("humn") = () => middle
      val result = monkeys("lzfc")()
      val target = monkeys("qrgn")()

      if result < target then end = middle - 1
      else if result > target then start = middle + 1
      else return middle
    }

    -1
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day21.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
