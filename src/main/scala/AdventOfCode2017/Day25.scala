package AdventOfCode2017

object Day25:
  case class Rule(write: Boolean, step: Int, next: String)
  case class Turing(tape: Set[Int], position: Int, state: String)

  def parse(input: Seq[String]): (String, Int, Map[(String, Boolean), Rule]) =
    val trimmed = input.map(_.init.trim.split(" "))
    val start = trimmed(0)(3)
    val total = trimmed(1)(5).toInt
    val rules = trimmed.drop(2).grouped(10).map { group =>
      val state = group(1)(2)
      Seq((state, false) -> parseRule(group.drop(3)), (state, true) -> parseRule(group.drop(7)))
    }
    (start, total, rules.flatten.toMap)

  def parseRule(block: Seq[Array[String]]): Rule =
    val write = block(0)(4) == "1"
    val step = if block(1)(6) == "right" then 1 else -1
    val next = block(2)(4)
    Rule(write, step, next)

  def step(rules: Map[(String, Boolean), Rule])(turing: Turing): Turing =
    val rule = rules(turing.state -> turing.tape.contains(turing.position))
    val nextTape = if rule.write then turing.tape + turing.position else turing.tape - turing.position
    Turing(nextTape, turing.position + rule.step, rule.next)

  def part1(input: Seq[String]): Int =
    val (start, total, rules) = parse(input)
    val initial = Turing(Set(), 0, start)
    Iterator.iterate(initial)(step(rules)).drop(total).next().tape.size

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day25.txt").getLines().toSeq
    println(part1(data))
