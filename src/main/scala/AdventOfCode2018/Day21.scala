package AdventOfCode2018

import scala.annotation.tailrec

object Day21:
  case class Instruction(opcode: String, a: Int, b: Int, c: Int)

  case class State(ip: Int, instructions: Seq[Instruction], register: Seq[Long]):
    def next: Option[State] = Option.when(instructions.indices.contains(register(ip)))(step)

    private def step: State =
      def gt(x: Long, y: Long) = if x > y then 1L else 0L
      def eq(x: Long, y: Long) = if x == y then 1L else 0L
      val Instruction(opcode, a, b, c) = instructions(register(ip).toInt)
      val nextC = opcode match
        case "addr" => register(a) + register(b)
        case "addi" => register(a) + b.toLong
        case "mulr" => register(a) * register(b)
        case "muli" => register(a) * b.toLong
        case "banr" => register(a) & register(b)
        case "bani" => register(a) & b.toLong
        case "borr" => register(a) | register(b)
        case "bori" => register(a) | b.toLong
        case "setr" => register(a)
        case "seti" => a.toLong
        case "gtir" => gt(a.toLong, register(b))
        case "gtri" => gt(register(a), b.toLong)
        case "gtrr" => gt(register(a), register(b))
        case "eqir" => eq(a.toLong, register(b))
        case "eqri" => eq(register(a), b.toLong)
        case "eqrr" => eq(register(a), register(b))
      val nextRegister = register.updated(c, nextC)
      copy(register = nextRegister.updated(ip, nextRegister(ip) + 1))
  end State

  def parse(input: Seq[String], registerZero: Long): State =
    val ip = input.head.filter(_.isDigit).toInt
    val instructions = input.tail.map(_.split(" ")).map {
      case Array(opcode, a, b, c) => Instruction(opcode, a.toInt, b.toInt, c.toInt)
    }
    State(ip, instructions, register = Seq(registerZero, 0L, 0L, 0L, 0L, 0L))

  // Just for fun, verify the answer by running the program step by step in the virtual machine implemented above.
  def part1(input: Seq[String]): Long =
    @tailrec
    def exec(state: State): Long = state.next match
      case Some(next) => exec(next)
      case None => state.register(0)

    exec(parse(input, 16134795L))
  end part1

  // Reverse engineering the code results in the algorithm below.
  // The outputs cycles so the result is the last value in the cycle.
  def part2: Long =
    def step(a: Long, b: Long): (Long, Long) =
      val nextA = (((a + (b & 0xFF)) & 0xFFFFFF) * 0x1016B) & 0xFFFFFF
      val nextB = b >> 8
      (nextA, nextB)

    @tailrec
    def helper(a: Long, previous: Long, seen: Set[Long]): Long =
      if seen.contains(a) then previous else
        val (nextA, _) = Iterator.iterate((0x10E660L, a | 0x10000))(step).drop(3).next()
        helper(nextA, a, seen + a)

    helper(0, 0, Set())
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day21.txt").getLines().toSeq
    println(part1(data))
    println(part2)
