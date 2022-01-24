package AdventOfCode2015

object Day07:
  sealed trait Gate
  case class Wire(in: String) extends Gate
  case class Not(in: String) extends Gate
  case class And(left: String, right: String) extends Gate
  case class Or(left: String, right: String) extends Gate
  case class LeftShift(in: String, amount: Int) extends Gate
  case class RightShift(in: String, amount: Int) extends Gate

  def parse(input: Seq[String]): Map[String, Gate] = input.map(_.split(" ")).map(a => a.last -> parseGate(a.dropRight(2))).toMap

  def parseGate(input: Array[String]): Gate = input match
    case Array(in) => Wire(in)
    case Array("NOT", in) => Not(in)
    case Array(left, "AND", right) => And(left, right)
    case Array(left, "OR", right) => Or(left, right)
    case Array(in, "RSHIFT", amount) => RightShift(in, amount.toInt)
    case Array(in, "LSHIFT", amount) => LeftShift(in, amount.toInt)

  def resolve(input: Seq[String]): Int =
    val gates = parse(input)
    val cache = collection.mutable.Map[String, Int]()
    def value(key: String): Int = cache.getOrElseUpdate(key, key.toIntOption.getOrElse(compute(key)))
    def compute(key: String): Int = gates(key) match
      case Wire(in) => value(in)
      case Not(in) => ~value(in) & 0xFFFF
      case And(left, right) => value(left) & value(right)
      case Or(left, right) => value(left) | value(right)
      case RightShift(in, amount) => value(in) >> amount
      case LeftShift(in, amount) => (value(in) << amount) & 0xFFFF
    value("a")

  def part1(input: Seq[String]): Int = resolve(input)

  def part2(input: Seq[String]): Int = resolve(input.appended("3176 -> b"))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day07.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
