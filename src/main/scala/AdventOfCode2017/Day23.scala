package AdventOfCode2017

object Day23:
  sealed trait State
  case object Running extends State
  case object Halted extends State

  case class Cpu(instructions: Seq[String], registers: Map[String, Long] = Map(), ip: Int = 0, state: State = Running, count: Int = 0):
    def next: Cpu = copy(ip = ip + 1)
    def read(key: String): Long = key.toLongOption.getOrElse(registers.getOrElse(key, 0L))
    def write(key: String, value: Long): Cpu = next.copy(registers = registers.updated(key, value))
    def step: Cpu =
      if !instructions.indices.contains(ip) then return copy(state = Halted)
      val Array(op, dest, src) = instructions(ip).split(" ")
      op match
        case "set" => write(dest, read(src))
        case "sub" => write(dest, read(dest) - read(src))
        case "mul" => write(dest, read(dest) * read(src)).copy(count = count + 1)
        case "jnz" => if read(dest) != 0 then copy(ip = ip + read(src).toInt) else next
  end Cpu

  def part1(input: Seq[String]): Int = Iterator.iterate(Cpu(input))(_.step).dropWhile(_.state == Running).next().count

  // Reverse engineering the code shows that it is counting composite (non-prime) numbers between two values.
  // The original code implements this with O(N^2) quadratic complexity for a total complexity of 10^13!
  def part2(input: Seq[String]): Int =
    def composite(n: Int): Boolean = (2 to math.sqrt(n).toInt).exists(n % _ == 0)
    (107900 to 124900 by 17).count(composite)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day23.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))