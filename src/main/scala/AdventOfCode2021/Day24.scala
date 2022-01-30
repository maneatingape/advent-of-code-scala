package AdventOfCode2021

// Much cleaner solution from reverse engineering the code.
// The original brute-force solution is preserved in the commit history.
object Day24:
  sealed trait Round
  case class Push(offset: Int) extends Round
  case class Pop(offset: Int) extends Round

  case class Constraint(index: Int, delta: Int):
    def min: Int = (1 + delta).max(1)
    def max: Int = (9 + delta).min(9)

  def parse(input: Seq[String]): List[Round] = input.grouped(18).toList.map { round =>
    def helper(n: Int) = round(n).split(" ").last.toInt
    if helper(4) == 1 then Push(helper(15)) else Pop(helper(5))
  }

  def constraints(rounds: List[Round]): List[Constraint] =
    def helper(remaining: List[(Round, Int)], stack: List[(Int, Int)], constraints: List[Constraint]): List[Constraint] =
      remaining match
        case Nil => constraints.sortBy(_.index)
        case (Push(firstValue), firstIndex) :: rest =>
          helper(remaining.tail, (firstValue, firstIndex) :: stack, constraints)
        case (Pop(secondValue), secondIndex) :: rest =>
          val (firstValue, firstIndex) = stack.head
          val delta = firstValue + secondValue
          helper(remaining.tail, stack.tail, Constraint(firstIndex, -delta) :: Constraint(secondIndex, delta) :: constraints)

    helper(rounds.zipWithIndex, Nil, Nil)
  end constraints

  def part1(input: Seq[String]): String = constraints(parse(input)).map(_.max).mkString

  def part2(input: Seq[String]): String = constraints(parse(input)).map(_.min).mkString

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day24.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
