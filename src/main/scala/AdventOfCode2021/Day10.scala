package AdventOfCode2021

object Day10:
  val opening = Map('(' -> 1, '[' -> 2, '{' -> 3, '<' -> 4)
  val closing = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val matching = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')

  type Checked = Either[Char, List[Char]]

  def check(line: String): Checked = line.foldLeft[Checked](Right(Nil)) {
    case (Right(stack), next) if opening.contains(next) => Right(next :: stack)
    case (Right(head :: tail), next) if matching(next) == head => Right(tail)
    case (Left(left), _) => Left(left)
    case (_, next) => Left(next)
  }

  def part1(input: Seq[String]): Long = input.map(check).collect { case Left(left) => closing(left) }.sum

  def part2(input: Seq[String]): Long =
    val results = input.map(check).collect { case Right(stack) =>
      stack.foldLeft(0L)((total, next) => 5 * total + opening(next))
    }
    results.sorted.apply(results.length / 2)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day10.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
