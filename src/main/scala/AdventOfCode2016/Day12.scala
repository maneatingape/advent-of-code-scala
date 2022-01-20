package AdventOfCode2016

object Day12:
  sealed trait State
  case object Running extends State
  case object Halted extends State

  case class Cpu(instructions: Seq[String], registers: Map[String, Int] = Map(), ip: Int = 0, state: State = Running):
    def next: Cpu = copy(ip = ip + 1)
    def read(key: String): Int = key.toIntOption.getOrElse(registers.getOrElse(key, 0))
    def write(key: String, value: Int): Cpu = next.copy(registers = registers.updated(key, value))
    def step: Cpu =
      if !instructions.indices.contains(ip) then return copy(state = Halted)
      val (op, src, dest) = instructions(ip).split(" ") match
        case Array(op, src, dest) => (op, src, dest)
        case Array(op, dest) => (op, "", dest)
      op match
        case "cpy" => write(dest, read(src))
        case "inc" => write(dest, read(dest) + 1)
        case "dec" => write(dest, read(dest) - 1)
        case "jnz" => if read(src) != 0 then copy(ip = ip + read(dest)) else next
  end Cpu

  def part1(input: Seq[String]): Int =
    Iterator.iterate(Cpu(input))(_.step).dropWhile(_.state == Running).next().registers("a")

  def part2(input: Seq[String]): Int =
    Iterator.iterate(Cpu(input, Map("c" -> 1)))(_.step).dropWhile(_.state == Running).next().registers("a")

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day12.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
