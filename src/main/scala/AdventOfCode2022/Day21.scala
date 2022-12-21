package AdventOfCode2022

object Day21:
  sealed trait Monkey
  case class Number(value: Long) extends Monkey
  case class Operation(left: String, operation: String, right: String) extends Monkey

  def parse(input: Seq[String]): Map[String, Monkey] =
    input.map {
      case s"$name: $left $operation $right" => name -> Operation(left, operation, right)
      case s"$name: $value" => name -> Number(value.toLong)
    }
    .toMap

  def calculate(monkeys: Map[String, Monkey]): Map[String, Long] =
    val result = collection.mutable.Map[String, Long]()
    def compute(name: String) = result.getOrElseUpdate(name, helper(name))
    def helper(name: String): Long = monkeys(name) match
      case Number(value) => value
      case Operation(left, operation, right) => operation match
        case "+" => compute(left) + compute(right)
        case "-" => compute(left) - compute(right)
        case "*" => compute(left) * compute(right)
        case "/" => compute(left) / compute(right)

    compute("root")
    result.toMap
  end calculate

  def part1(input: Seq[String]): Long =
    val monkeys = parse(input)
    val results = calculate(monkeys)
    results("root")

  def part2(input: Seq[String]): Long =
    val monkeys = parse(input)
    val results = calculate(monkeys)

    def helper(name: String, value: Long): Option[Long] = monkeys(name) match
      case Number(_) => Option.when(name == "humn")(value)
      case Operation(left, _, right) if name == "root" =>
        val first = helper(right, results(left))
        val second = helper(left, results(right))
        first.orElse(second)
      case Operation(left, operation, right) =>
        val first = operation match
          case "+" => helper(right, value - results(left))
          case "-" => helper(right, results(left) - value)
          case "*" => if results(left) != 0 then helper(right, value / results(left)) else None
          case "/" => if value != 0 then helper(right, results(left) / value) else None
        val second = operation match
          case "+" => helper(left, value - results(right))
          case "-" => helper(left, value + results(right))
          case "*" => if results(right) != 0 then helper(left, value / results(right)) else None
          case "/" => helper(left, value * results(right))
        first.orElse(second)

    helper("root", -1).get
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day21.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
