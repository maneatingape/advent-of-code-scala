package AdventOfCode2016

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
      val (op, src, dest) = instructions(ip).split(" ") match
        case Array(op, src, dest) => (op, src, dest)
        case Array(op, dest) => (op, "", dest)
        case Array(op) => (op, "", "")
      op match
        case "cpy" => write(dest, read(src))
        case "inc" => write(dest, read(dest) + 1)
        case "dec" => write(dest, read(dest) - 1)
        case "jnz" => if read(src) != 0 then copy(ip = ip + read(dest)) else next
        case "tgl" =>
          val index = ip + read(dest)
          if !instructions.indices.contains(index) then next else
            val tokens = instructions(ip + read(dest)).split(" ")
            if tokens.size == 2 then
              val nextOp = if tokens.head == "inc" then "dec" else "inc"
              val nextInstructions = instructions.updated(index, Seq(nextOp, tokens(1)).mkString(" "))
              next.copy(instructions = nextInstructions)
            else
              val nextOp = if tokens.head == "jnz" then "cpy" else "jnz"
              val nextInstructions = instructions.updated(index, Seq(nextOp, tokens(1), tokens(2)).mkString(" "))
              next.copy(instructions = nextInstructions)
        // Unofficial CPU upgrade
        case "nop" => next
        case "mul" => write(dest, read(src) * read(dest))
  end Cpu

  // Reverse engineering the code shows that it is calculating factorial(a) + (71*75)
  // Two inner loops implement multiplication in linear time.
  // Patch these with extended instructions to implement the multiplication in constant time.
  def patch(input: Seq[String]): Seq[String] = input
    // Patch 1
    .updated(2, "nop")
    .updated(3, "nop")
    .updated(4, "nop")
    .updated(5, "nop")
    .updated(6, "nop")
    .updated(7, "nop")
    .updated(8, "nop")
    .updated(9, "mul b a")
    // Patch 2
    .updated(12, "nop")
    .updated(13, "nop")
    .updated(14, "mul 2 c")

  def run(input: Seq[String], eggs: Int): Int =
    Iterator.iterate(Cpu(input, Map("a" -> eggs)))(_.step).dropWhile(_.state == Running).next().registers("a")

  def part1(input: Seq[String]): Int = run(input, 7)

  def part2(input: Seq[String]): Int = run(patch(input), 12)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day23.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
