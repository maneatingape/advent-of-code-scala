package AdventOfCode2021

object Day04:
  val indices =
    val rows = Seq.tabulate(5, 5)((row, column) => 5 * row + column)
    val columns = rows.transpose
    rows ++ columns

  case class Win(score: Int, turn: Int)

  case class Board(numbers: Seq[Int], win: Option[Win] = None):
    def mark(number: Int, turn: Int): Board =
      val nextNumbers = numbers.map(n => if n == number then 0 else n)
      val isWin = indices.exists(_.forall(i => nextNumbers(i) == 0))
      val nextWin = Option.when(isWin)(Win(nextNumbers.sum * number, turn))
      Board(nextNumbers, win.orElse(nextWin))

  def play(input: Seq[String]): Seq[Win] =
    val numbers = input.head.split(",").map(_.toInt).toSeq
    val boards = input.tail.grouped(6).toSeq.map(s => Board(s.mkString(" ").split(" ").flatMap(_.toIntOption).toSeq))
    numbers
      .zipWithIndex
      .foldLeft(boards) { case (boards, (number, turn)) => boards.map(_.mark(number, turn)) }
      .flatMap(_.win)

  def part1(input: Seq[String]): Int = play(input).minBy(_.turn).score

  def part2(input: Seq[String]): Int = play(input).maxBy(_.turn).score

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day04.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
