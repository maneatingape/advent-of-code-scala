package AdventOfCode2020

object Day19:
  type Rules = Map[Int, Rule]

  sealed trait Rule:
    def prefix(line: String)(using rules: Rules): Option[Int]

  case class Letter(letter: String) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] = Option.when(line.take(1) == letter)(1)

  case class Redirect(key : Int) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] = rules(key).prefix(line)

  case class Sequence(first: Int, second: Int) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] =
      rules(first).prefix(line).flatMap(length => rules(second).prefix(line.drop(length)).map(_ + length))

  case class TripleSequence(first: Int, second: Int, third: Int) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] =
      Sequence(first, second).prefix(line).flatMap(length => rules(third).prefix(line.drop(length)).map(_ + length))

  case class SingleChoice(first: Int, second: Int) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] =
      rules(first).prefix(line).orElse(rules(second).prefix(line))

  case class DoubleChoice(first: Int, second: Int, third: Int, fourth: Int) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] =
      Sequence(first, second).prefix(line).orElse(Sequence(third, fourth).prefix(line))

  case class RuleZero(size: Int) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] =
      var index = line.length - size
      var length = 3 * size
      var found = false
      while length <= line.length && found == false do
        val left = line.take(index).grouped(size).forall(group => rules(42).prefix(group) == Some(size))
        val right = line.drop(index).grouped(size).forall(group => rules(31).prefix(group) == Some(size))
        index -= size
        length += 2 * size
        found = left && right
      Option.when(found)(line.length)
    end prefix

  object Parse:
    val letter = "\"(\\w)\"".r
    val redirect = "(\\d+)".r
    val sequence = "(\\d+) (\\d+)".r
    val tripleSequence = "(\\d+) (\\d+) (\\d+)".r
    val singleChoice = "(\\d+) \\| (\\d+)".r
    val doubleChoice = "(\\d+) (\\d+) \\| (\\d+) (\\d+)".r

  def parse(input: Seq[String]): (Rules, Seq[String]) =
    val index = input.indexOf("")
    val rules = parseRules(input.take(index))
    val messages = input.drop(index + 1)
    (rules, messages)

  def parseRules(input: Seq[String]): Rules =
    val rules = input.map { line =>
      val Array(id, pattern) = line.split(": ")
      val rule = pattern match
        case Parse.letter(letter) => Letter(letter)
        case Parse.redirect(first) => Redirect(first.toInt)
        case Parse.sequence(first, second) => Sequence(first.toInt, second.toInt)
        case Parse.tripleSequence(first, second, third) => TripleSequence(first.toInt, second.toInt, third.toInt)
        case Parse.singleChoice(first, second) => SingleChoice(first.toInt, second.toInt)
        case Parse.doubleChoice(first, second, third, fourth) => DoubleChoice(first.toInt, second.toInt, third.toInt, fourth.toInt)
      id.toInt -> rule
    }
    rules.toMap

  def part1(input: Seq[String]): Int =
    val (rules, messages) = parse(input)
    given Rules = rules
    messages.count(message => rules(0).prefix(message).exists(_ == message.length))

  def part2(input: Seq[String], size: Int = 8): Int =
    val (original, messages) = parse(input)
    val rules = original.updated(0, RuleZero(size))
    given Rules = rules
    messages.count(message => rules(0).prefix(message).exists(_ == message.length))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day19.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
