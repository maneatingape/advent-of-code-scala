package AdventOfCode2021

object Day04:
  case class Win(score: Int, turn: Int)

  object Board:
    val indices =
      val rows = Seq.tabulate(5, 5)((row, column) => 5 * row + column)
      val columns = Seq.tabulate(5, 5)((row, column) => row + 5 * column)
      rows ++ columns

    def apply(numbers: Seq[Int]) = new Board(numbers, Array.fill(25)(false), None)

  case class Board(numbers: Seq[Int], marked: Array[Boolean], score: Option[Win]):
    def mark(number: Int, turn: Int): Board =
      var score = this.score

      if score.isEmpty then
        val index = numbers.indexOf(number)
        if index >= 0 then
          marked(index) = true
          if Board.indices.exists(_.forall(i => marked(i))) then
            val unmarkedSum = numbers.zipWithIndex.map((n,i) => if marked(i) then 0 else n).sum
            score = Some(Win(unmarkedSum * number, turn))

      Board(numbers, marked, score)
    end mark

  def play(input: Seq[String]): Seq[Win] =
    val numbers = input.head.split(",").map(_.toInt).toSeq
    val boards = input.tail.grouped(6).toSeq.map { xs =>
      Board(xs.mkString(" ").split(" ").filter(!_.isEmpty).map(_.toInt).toSeq)
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
