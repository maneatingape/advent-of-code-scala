package AdventOfCode2021

object Day04:
  case class Win(score: Int, turn: Int)

  object Board:
    val indices =
      val rows = Seq.tabulate(5, 5)((row, column) => 5 * row + column)
      val columns = Seq.tabulate(5, 5)((row, column) => row + 5 * column)
      rows ++ columns

  case class Board(numbers: Seq[Int], score: Option[Win]):
    def mark(number: Int, turn: Int): Board =
      val nextNumbers = numbers.map(n => if n == number then 0 else n)
      val nextScore = Option.when(Board.indices.exists(_.forall(i => nextNumbers(i) == 0)))(Win(nextNumbers.sum * number, turn))
      Board(nextNumbers, score.orElse(nextScore))

  def play(input: Seq[String]): Seq[Win] =
    val numbers = input.head.split(",").map(_.toInt).toSeq
    val boards = input.tail.grouped(6).toSeq.map { xs =>
      Board(xs.mkString(" ").split(" ").filter(!_.isEmpty).map(_.toInt).toSeq, None)
    }
    numbers
      .zipWithIndex
      .foldLeft(boards) { case (boards, (number, index)) =>
        boards.map(_.mark(number, index))
      }
      .flatMap(_.score)

  def part1(input: Seq[String]): Int = play(input).minBy(_.turn).score

  def part2(input: Seq[String]): Int = play(input).maxBy(_.turn).score

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day04.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
