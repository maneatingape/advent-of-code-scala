package AdventOfCode2019

import Day09.IntCode
import scala.annotation.tailrec

object Day25:
  def description(computer: IntCode): IntCode =
    @tailrec
    def helper(computer: IntCode, message: String): IntCode =
      if message.endsWith("Command?\n") || message.endsWith("!\n\n") || message.endsWith("\"\n") then
        print(message)
        computer
      else
        val nextComputer = computer.next
        val nextMessage = nextComputer.result match
          case IntCode.Output(value) => message.appended(value.toChar)
          case _ => message
        helper(nextComputer, nextMessage)
    end helper

    helper(computer, "")
  end description

  def part1(memory: Seq[Long], precanned: Seq[String]): Unit =
    @tailrec
    def helper(computer: IntCode, command: String, precanned: Seq[String]): Unit =
      if command == "exit" then () else
        val input = command.map(_.toLong).appended(10L)
        val nextComputer = description(computer.withInput(input*))

        val (nextCommand, nextPrecanned) = precanned match
          case first +: tail => (first, tail)
          case _ => (Console.in.readLine(), Seq())

        helper(nextComputer, nextCommand, nextPrecanned)
    end helper

    helper(IntCode(memory) ,"", precanned)
  end part1

  def main(args: Array[String]): Unit =
    // Uncomment 2nd line to solve automatically, leave blank to play game interactively
    val solution = "d2VzdAp3ZXN0CnRha2UgYm93bCBvZiByaWNlCmVhc3QKbm9ydGgKZWFzdApzb3V0aAp0YWtlIGRhcmsgbWF0dGVyCm5vcnRoCndlc3QKbm9ydGgKdGFrZSBjYW5keSBjYW5lCndlc3QKd2VzdApub3J0aAp0YWtlIGRlaHlkcmF0ZWQgd2F0ZXIKd2VzdApzb3V0aA=="
    val precanned = Seq() // ++ String(java.util.Base64.getDecoder.decode(solution)).split("\n").toSeq

    val data = io.Source.fromResource("AdventOfCode2019/Day25.txt").mkString.trim.split(",").map(_.toLong).toSeq
    part1(data, precanned)
  end main
