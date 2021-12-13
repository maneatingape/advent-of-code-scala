package AdventOfCode2020

object Day08:
  sealed trait Instruction(val acc: Int, val ip: Int)
  case class Acc(amount: Int) extends Instruction(amount, 1)
  case class Jmp(amount: Int) extends Instruction(0, amount)
  case class Nop(amount: Int) extends Instruction(0, 1)

  sealed trait State
  case class Running(acc: Int, ip: Int, visited: List[Int]) extends State
  case class Infinite(acc: Int) extends State
  case class Halted(acc: Int) extends State

  def parse(input: Seq[String]): Seq[Instruction] = input.map { line =>
    line.split(" ") match
      case Array("acc", amount) => Acc(amount.toInt)
      case Array("jmp", amount) => Jmp(amount.toInt)
      case Array("nop", amount) => Nop(amount.toInt)
  }

  def execute(program: Seq[Instruction]): State =
    def next(state: State) = Some((state, state))
    Iterator.unfold[State, State](Running(0, 0, Nil)) {
      case Running(acc, ip, visited) =>
        if ip >= program.length then next(Halted(acc))
        else if visited.contains(ip) then next(Infinite(acc))
        else next(Running(acc + program(ip).acc, ip + program(ip).ip, ip :: visited))
      case _ => None
    }
    .toSeq.last

  def part1(input: Seq[String]): Int =
    val program = parse(input)
    List(program)
      .map(execute)
      .collectFirst { case Infinite(acc) => acc }
      .get

  def part2(input: Seq[String]): Int =
    val program = parse(input)
    program
      .zipWithIndex
      .collect {
        case (Jmp(amount), index) => program.updated(index, Nop(amount))
        case (Nop(amount), index) => program.updated(index, Jmp(amount))
      }
      .map(execute)
      .collectFirst { case Halted(acc) => acc }
      .get

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day08.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
