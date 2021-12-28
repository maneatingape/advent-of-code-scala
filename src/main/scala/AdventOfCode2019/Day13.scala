package AdventOfCode2019

import scala.annotation.tailrec

object Day13:
  object IntCode:
    val powers = Map(1 -> 100, 2 -> 1000, 3 -> 10000)

    sealed trait State
    case object Initial extends State
    case object Running extends State
    case object Halted extends State
    case class Output(value: Long) extends State

    def apply(input: Seq[Long]): IntCode =
      val memory = input.zipWithIndex.map((value, index) => index.toLong -> value)
      IntCode(0, 0, memory.toMap.withDefaultValue(0), Seq(), Initial)
  end IntCode

  case class IntCode(ip: Long, relativeBase: Long, memory: Map[Long, Long], input: Seq[Long], result: IntCode.State):
    import IntCode._

    private def next: IntCode = memory(ip) % 100 match
      case 1 => copy(ip = ip + 4, memory = write(3, read(1) + read(2)), result = Running)                   // Add
      case 2 => copy(ip = ip + 4, memory = write(3, read(1) * read(2)), result = Running)                   // Multiply
      case 3 => copy(ip = ip + 2, memory = write(1, input.head), input = input.tail, result = Running)      // Read
      case 4 => copy(ip = ip + 2, result = Output(read(1)))                                                 // Write
      case 5 => copy(ip = if read(1) != 0 then read(2) else ip + 3, result = Running)                       // Jump if true
      case 6 => copy(ip = if read(1) == 0 then read(2) else ip + 3, result = Running)                       // Jump if false
      case 7 => copy(ip = ip + 4, memory = write(3, if read(1) < read(2) then 1 else 0), result = Running)  // Less than
      case 8 => copy(ip = ip + 4, memory = write(3, if read(1) == read(2) then 1 else 0), result = Running) // Equals
      case 9 => copy(ip = ip + 2, relativeBase = relativeBase + read(1), result = Running)                  // Relative base
      case 99 => copy(result = Halted)                                                                      // Halt

    private def read(offset: Int): Long = (memory(ip) / powers(offset)) % 10 match
      case 0 => memory(memory(ip + offset))
      case 1 => memory(ip + offset)
      case 2 => memory(relativeBase + memory(ip + offset))

    private def write(offset: Int, value: Long): Map[Long, Long] = (memory(ip) / powers(offset)) % 10 match
      case 0 => memory.updated(memory(ip + offset), value)
      case 2 => memory.updated(relativeBase + memory(ip + offset), value)

    def withInput(next: Long*): IntCode = copy(input = next)

    def nextOutput: IntCode = Iterator.iterate(next)(_.next).dropWhile(_.result == Running).next()

    def allOutput: Seq[Long] =
      val output = Iterator.iterate(this)(_.nextOutput).takeWhile(_.result != Halted)
      output.toSeq.map(_.result).collect { case Output(value) => value }
  end IntCode

  object Ansi:
    val Home = "\u001b[H"
    val Clear = "\u001b[2J"
    val Bold = "\u001b[1m"
    val Reset = "\u001b[0m"
    val Green = "\u001b[32m"
    val Yellow = "\u001b[33m"
    val Blue = "\u001b[34m"
    val White = "\u001b[97m"

    val sprites = Map(
      0L -> " ",
      1L -> (Green + "#"),
      2L -> (Blue + "o"),
      3L -> (White + Bold + "=" + Reset),
      4L -> (Yellow + Bold + "." + Reset))

    def echo(score: Long, blocks: Long, tiles: Map[Point, Long]): Unit =
      val buffer = collection.mutable.ListBuffer(Home + Clear)
      buffer += s"$White$Bold  Score: $score  Blocks: $blocks $Reset\n"

      for y <- 0 until 20 do
        for x <- 0 until 44 do
          buffer += sprites(tiles(Point(x,y)))
        buffer += "\n"
      buffer += Reset

      println(buffer.mkString)
      Thread.sleep(25)
    end echo
  end Ansi

  case class Point(x: Long, y: Long)

  def part1(memory: Seq[Long]): Int = IntCode(memory).allOutput.grouped(3)
    .foldLeft(Map.empty[Point, Long]) { case (tiles, Seq(x, y, id)) => tiles.updated(Point(x, y), id) }
    .values.count(_ == 2)

  def part2(memory: Seq[Long]): Long =
    def take(computer: IntCode, amount: Int): (IntCode, Seq[Long]) =
      val raw = Iterator.iterate(computer)(_.nextOutput).drop(1).take(amount).toSeq
      val output = raw.map(_.result).collect { case IntCode.Output(value) => value }
      (raw.last, output)

    def step(score: Long, paddle: Long, ball: Long, tiles: Map[Point, Long], output: Seq[Long]): (Long, Long, Long, Map[Point, Long]) =
      output.grouped(3).foldLeft((score, paddle, ball, tiles)) { case ((score, paddle, ball, tiles), Seq(x, y, id)) =>
        val nextScore = if x == -1 then id else score
        val nextPaddle = if id == 3 then x else paddle
        val nextBall = if id == 4 then x else ball
        (nextScore, nextPaddle, nextBall, tiles.updated(Point(x, y), id))
      }

    @tailrec
    def play(computer: IntCode, amount: Int, score: Long, paddle: Long, ball: Long, tiles: Map[Point, Long]): Long =
      val (nextComputer, output) = take(computer, amount)
      val (nextScore, nextPaddle, nextBall, nextTiles) = step(score, paddle, ball, tiles, output)

      val finalComputer = nextComputer.withInput((nextBall - nextPaddle).sign)
      val nextBlocks = nextTiles.values.count(_ == 2)

      // Not essential but hilarious
      // Run program on the command line to observe an animated game of breakout
      Ansi.echo(nextScore, nextBlocks, nextTiles)

      if nextBlocks == 0 then nextScore else play(finalComputer, 6, nextScore, nextPaddle, nextBall, nextTiles)
    end play

    play(IntCode(memory.updated(0, 2)), 2643, -1, -1, -1, Map())
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day13.txt").mkString.trim.split(",").map(_.toLong).toSeq
    println(part1(data))
    println(part2(data))
