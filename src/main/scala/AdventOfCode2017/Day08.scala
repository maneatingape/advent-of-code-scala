package AdventOfCode2017

object Day08:
  def compute(input: Seq[String]): (Map[String, Int], Int) =
    val initial = Map.empty[String, Int].withDefaultValue(0) -> 0
    input.map(_.split(" ")).foldLeft(initial) { case ((registers, max), instruction) =>
      val Array(destination, operation, amount, _, test, condition, threshold) = instruction

      val left = registers(test)
      val right = threshold.toInt
      val valid = condition match
        case ">" => left > right
        case "<" => left < right
        case ">=" => left >= right
        case "<=" => left <= right
        case "==" => left == right
        case "!=" => left != right

      val modifier = if valid then if operation == "inc" then 1 else -1 else 0
      val next = registers(destination) + amount.toInt * modifier
      registers.updated(destination, next) -> max.max(next)
    }

  def part1(input: Seq[String]): Int = compute(input)._1.values.max

  def part2(input: Seq[String]): Int = compute(input)._2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day08.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
