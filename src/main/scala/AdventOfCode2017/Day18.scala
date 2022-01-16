package AdventOfCode2017

import scala.annotation.tailrec

object Day18:
  sealed trait State
  case object Running extends State
  case object Waiting extends State

  case class Cpu(
    instructions: Seq[String],
    rcv: (Cpu, String) => Cpu,
    registers: Map[String, Long] = Map(),
    state: State = Running,
    ip: Int = 0,
    received: Int = 0,
    input: Seq[Long] = Seq(),
    output: Seq[Long] = Seq()):

    def next: Cpu = copy(ip = ip + 1)
    def read(key: String): Long = key.toLongOption.getOrElse(registers.getOrElse(key, 0L))
    def write(key: String, value: Long): Cpu = next.copy(registers = registers.updated(key, value))

    def step: Cpu =
      val (op, dest, src) = instructions(ip).split(" ") match
        case Array(op, dest, src) => (op, dest, src)
        case Array(op, dest) => (op, dest, "")
      op match
        case "set" => write(dest, read(src))
        case "add" => write(dest, read(dest) + read(src))
        case "mul" => write(dest, read(dest) * read(src))
        case "mod" => write(dest, read(dest) % read(src))
        case "jgz" => if read(dest) > 0 then copy(ip = ip + read(src).toInt) else next
        case "snd" => next.copy(output = output.appended(read(dest)))
        case "rcv" => rcv(this, dest)
  end Cpu

  def part1(input: Seq[String]): Long =
    def rcv(cpu: Cpu, src: String): Cpu = if cpu.read(src) != 0 then cpu.copy(state = Waiting) else cpu.next
    Iterator.iterate(Cpu(input, rcv))(_.step).dropWhile(_.state == Running).next().output.last

  def part2(input: Seq[String]): Int =
    def rcv(cpu: Cpu, dest: String): Cpu =
      if cpu.received == cpu.input.size then cpu.copy(state = Waiting)
      else cpu.write(dest, cpu.input(cpu.received)).copy(received = cpu.received + 1)

    @tailrec
    def helper(program0: Cpu, program1: Cpu): Int = (program0.state, program1.state) match
      case (Waiting, Waiting) => program1.output.size
      case _ => helper(program0.copy(input = program1.output).step, program1.copy(input = program0.output).step)

    helper(Cpu(input, rcv, Map("p" -> 0L)), Cpu(input, rcv, Map("p" -> 1L)))
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day18.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
