package AdventOfCode2019

import scala.annotation.tailrec

object Day07:
  object IntCode:
    val powers = Map(1 -> 100, 2 -> 1000)

    sealed trait State
    case object Initial extends State
    case object Running extends State
    case object Halted extends State
    case class Output(value: Int) extends State

    def apply(code: Seq[Int]): IntCode = IntCode(0, code, Seq(), Initial)
  end IntCode

  case class IntCode(ip: Int, code: Seq[Int], input: Seq[Int], result: IntCode.State):
    import IntCode._

    private def next: IntCode = code(ip) % 100 match
      case 1 => IntCode(ip + 4, write(3, read(1) + read(2)), input, Running)                    // Add
      case 2 => IntCode(ip + 4, write(3, read(1) * read(2)), input, Running)                    // Multiply
      case 3 => IntCode(ip + 2, write(1, input.head), input.tail, Running)                      // Read
      case 4 => IntCode(ip + 2, code, input, Output(read(1)))                                   // Write
      case 5 => IntCode(if read(1) != 0 then read(2) else ip + 3, code, input, Running)         // Jump if true
      case 6 => IntCode(if read(1) == 0 then read(2) else ip + 3, code, input, Running)         // Jump if false
      case 7 => IntCode(ip + 4, write(3, if read(1) < read(2) then 1 else 0), input, Running)   // Less than
      case 8 => IntCode(ip + 4, write(3, if read(1) == read(2) then 1 else 0), input, Running)  // Equals
      case 99 => IntCode(ip, code, input, Halted)                                               // Halt

    private def read(offset: Int): Int = (code(ip) / powers(offset)) % 10 match
      case 0 => code(code(ip + offset))
      case 1 => code(ip + offset)

    private def write(offset: Int, value: Int): Seq[Int] = code.updated(code(ip + offset), value)

    def withInput(next: Int*): IntCode = copy(input = input.appendedAll(next))

    def nextOutput: IntCode = Iterator.iterate(next)(_.next).dropWhile(_.result == Running).next()

    def allOutput: Seq[Int] =
      val output = Iterator.iterate(this)(_.nextOutput).takeWhile(_.result != Halted)
      output.toSeq.map(_.result).collect { case Output(value) => value }
  end IntCode

  def part1(code: Seq[Int]): Int = (0 to 4).permutations.map(_.foldLeft(0)((total, next) => IntCode(code).withInput(next, total).allOutput.last)).max

  def part2(code: Seq[Int]): Int =
    @tailrec
    def helper(previousOutput: Int)(amps: Seq[IntCode]): Int =
      val nextAmp = amps.head.withInput(previousOutput).nextOutput
      nextAmp.result match
        case IntCode.Output(nextOutput) => helper(nextOutput)(amps.tail.appended(nextAmp))
        case _ => previousOutput

    (5 to 9).map(phase => IntCode(code).withInput(phase)).permutations.map(helper(0)).max
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day07.txt").mkString.trim.split(",").map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
