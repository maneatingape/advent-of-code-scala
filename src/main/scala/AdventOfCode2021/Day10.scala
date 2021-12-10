package AdventOfCode2021

object Day10:
  val opening = Map('(' -> 1, '[' -> 2, '{' -> 3, '<' -> 4)
  val closing = Map('x' -> 0, ')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val matching = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')

  def check(line: String): (List[Char], Char ) =
    line.foldLeft((List('x'), 'x')) { case ((stack, state), next) =>
      if state != 'x' then (stack, state)
      else if opening.contains(next) then (next :: stack, state)
      else if matching(next) == stack.head then (stack.tail, state)
      else (stack, next)
    }

  def part1(input: Seq[String]): Long = input.map(check).map(_._2).map(closing).sum

  def part2(input: Seq[String]): Long =
    val results = input.map(check).collect { case (stack, 'x') =>
      stack.dropRight(1).foldLeft(0L)((total, next) => 5 * total + opening(next))
    }
    results.sorted.apply(results.length / 2)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day10.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
