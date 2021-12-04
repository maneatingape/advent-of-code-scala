package AdventOfCode2021

object Day04:
  object Board:
    val indices =
      val rows = Seq.tabulate(5, 5)((row, column) => 5 * row + column)
      val columns = Seq.tabulate(5, 5)((row, column) => row + 5 * column)
      rows ++ columns

  class Board(val numbers: Seq[Int]):
    val marked: Array[Boolean] = Array.fill(25)(false)
    var score = Option.empty[Int]

    def mark(number: Int): Unit =
      if score.isEmpty then
        val index = numbers.indexOf(number)
        if index >= 0 then
          marked(index) = true
          val win = Board.indices.exists(_.forall(i => marked(i)))
          if win then
            val unmarkedSum = numbers.zipWithIndex.map((n,i) => if marked(i) then 0 else n).sum
            score = Some(unmarkedSum * number)

  def parseNumbersAndBoards(input: Seq[String]): (Seq[Int], Seq[Board]) =
    val numbers = input.head.split(",").map(_.trim.toInt).toSeq
    val boards = input.tail.grouped(6).toSeq.map { xs =>
      new Board(xs.mkString(" ").split(" ").filter(!_.isEmpty).map(_.toInt).toSeq)
    }
    (numbers, boards)

  def part1(input: Seq[String]): Int =
    def firstWinner(numbers: Seq[Int], boards: Seq[Board]): Int =
      boards.foreach(_.mark(numbers.head))
      boards
        .find(_.score.isDefined)
        .flatMap(_.score)
        .getOrElse(firstWinner(numbers.tail, boards))

    val (numbers, boards) = parseNumbersAndBoards(input)
    firstWinner(numbers, boards)

  def part2(input: Seq[String]): Int =
    def lastWinner(numbers: Seq[Int], boards: Seq[Board]): Int = boards match
      case Seq(head) =>
        numbers.foreach(head.mark)
        head.score.get
      case _ =>
        boards.foreach(_.mark(numbers.head))
        lastWinner(numbers.tail, boards.filter(_.score.isEmpty))

    val (numbers, boards) = parseNumbersAndBoards(input)
    lastWinner(numbers, boards)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day04.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
