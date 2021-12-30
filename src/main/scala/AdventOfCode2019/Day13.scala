package AdventOfCode2019

import scala.annotation.tailrec
import Day09.IntCode

object Day13:
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
