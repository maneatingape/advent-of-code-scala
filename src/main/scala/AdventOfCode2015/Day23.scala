package AdventOfCode2015

object Day23:
  sealed trait State
  case object Running extends State
  case object Halted extends State

  case class Cpu(instructions: Seq[String], registers: Map[String, Int] = Map(), ip: Int = 0, state: State = Running):
    def next: Cpu = copy(ip = ip + 1)
    def read(key: String): Int = key.toIntOption.getOrElse(registers.getOrElse(key, 0))
    def write(key: String, value: Int): Cpu = next.copy(registers = registers.updated(key, value))
    def step: Cpu =
      if !instructions.indices.contains(ip) then return copy(state = Halted)
      val (op, src, dest) = instructions(ip).split("[, ]+") match
        case Array(op, src, dest) => (op, src, dest)
        case Array(op, dest) => (op, "", dest)
      op match
        case "hlf" => write(dest, read(dest) / 2)
        case "tpl" => write(dest, read(dest) * 3)
        case "inc" => write(dest, read(dest) + 1)
        case "jmp" => copy(ip = ip + read(dest))
        case "jie" => if read(src) % 2 == 0 then copy(ip = ip + read(dest)) else next
        case "jio" => if read(src) == 1 then copy(ip = ip + read(dest)) else next
  end Cpu

  // Reverse engineering the code shows that it calculates the length of the
  // 3n + 1 sequence (https://en.wikipedia.org/wiki/Collatz_conjecture)
  // for one of two different numbers chosen depending on whether a is 0 or 1.
  def run(input: Seq[String], a: Int): Int =
    Iterator.iterate(Cpu(input, Map("a" -> a)))(_.step).dropWhile(_.state == Running).next().registers("b")

  def part1(input: Seq[String]): Int = run(input, 0)

  def part2(input: Seq[String]): Int = run(input, 1)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day23.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
