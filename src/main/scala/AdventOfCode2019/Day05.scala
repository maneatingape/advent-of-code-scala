package AdventOfCode2019

import scala.annotation.tailrec

object Day05:
  val powers = Map(1 -> 100, 2 -> 1000)

  def mode(ip: Int, code: Seq[Int], offset: Int): Int = (code(ip) / powers(offset) % 10) match
    case 0 => code(code(ip + offset))
    case 1 => code(ip + offset)
  end mode

  @tailrec
  def exec(ip: Int, code: Seq[Int], input: Seq[Int], output: Seq[Int]): Seq[Int] = (code(ip) % 100) match
    // Add
    case 1 =>
      val nextCode = code.updated(code(ip + 3), mode(ip, code, 1) + mode(ip, code, 2))
      exec(ip + 4, nextCode, input, output)
    // Multiple
    case 2 =>
      val nextCode = code.updated(code(ip + 3), mode(ip, code, 1) * mode(ip, code, 2))
      exec(ip + 4, nextCode, input, output)
    // Read
    case 3 =>
      val nextCode = code.updated(code(ip + 1), input.head)
      exec(ip + 2, nextCode, input.tail, output)
    // Write
    case 4 =>
      val outputValue = mode(ip, code, 1)
      exec(ip + 2, code, input, output.appended(outputValue))
    // Jump if true
    case 5 =>
      val nextIp = if mode(ip, code, 1) != 0 then mode(ip, code, 2) else ip + 3
      exec(nextIp, code, input, output)
    // Jump if false
    case 6 =>
      val nextIp = if mode(ip, code, 1) == 0 then mode(ip, code, 2) else ip + 3
      exec(nextIp, code, input, output)
    // Less than
    case 7 =>
      val nextCode = code.updated(code(ip + 3), if mode(ip, code, 1) < mode(ip, code, 2) then 1 else 0)
      exec(ip + 4, nextCode, input, output)
    // Equals
    case 8 =>
      val nextCode = code.updated(code(ip + 3), if mode(ip, code, 1) == mode(ip, code, 2) then 1 else 0)
      exec(ip + 4, nextCode, input, output)
    case 99 => output
  end exec

  def part1(input: Seq[Int]): Int = exec(0, input, Seq(1), Seq()).last

  def part2(input: Seq[Int]): Int = exec(0, input, Seq(5), Seq()).last

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day05.txt").mkString.trim.split(",").map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
