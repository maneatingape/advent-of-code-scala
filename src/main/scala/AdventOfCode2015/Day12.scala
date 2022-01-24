package AdventOfCode2015

object Day12:
  sealed trait Json
  case class JNumber(value: Int) extends Json
  case class JString(value: String) extends Json
  case class JArray(values: Seq[Json]) extends Json
  case class JObject(values: Seq[Json]) extends Json

  val numberRegex = "(-?\\d+)(.*)".r
  val stringRegex = "\"(\\w+)\"(.*)".r

  def parse(input: String): (Json, String) = input match
    case numberRegex(digits, rest) => (JNumber(digits.toInt), rest)
    case stringRegex(string, rest) => (JString(string), rest)
    case _ => input.head match
      case '[' => parseArray(JArray(Seq()), input.tail)
      case '{' => parseObject(JObject(Seq()), input.tail)

  def parseArray(current: JArray, input: String): (Json, String) =
    val (value, remaining) = parse(input)
    val next = JArray(current.values.appended(value))
    if remaining.head == ']' then (next, remaining.tail) else parseArray(next, remaining.tail)

  def parseObject(current: JObject, input: String): (Json, String) =
    val (key, first) = parse(input)
    val (value, remaining) = parse(first.tail)
    val next = JObject(current.values.appended(value))
    if remaining.head == '}' then (next, remaining.tail) else parseObject(next, remaining.tail)

  def part1(input: String): Int = "-?\\d+".r.findAllIn(input).map(_.toInt).sum

  def part2(input: String): Int =
    def helper(json: Json): Int = json match
      case JNumber(value) => value
      case JString(_) => 0
      case JArray(values) => values.map(helper).sum
      case JObject(values) => if values.contains(JString("red")) then 0 else values.map(helper).sum

    helper(parse(input)._1)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day12.txt").mkString.trim
    println(part1(data))
    println(part2(data))
