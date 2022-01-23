package AdventOfCode2016

object Day25:
  sealed trait State
  case object Running extends State
  case object Halted extends State
  case class Output(value: Int) extends State

  case class Cpu(instructions: Seq[String], registers: Map[String, Int] = Map(), ip: Int = 0, state: State = Running):
    def next: Cpu = copy(ip = ip + 1, state = Running)
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
        case "jnz" => if read(src) != 0 then copy(ip = ip + read(dest), state = Running) else next
        case "out" => next.copy(state = Output(read(dest)))
  end Cpu

  // Reverse engineering the code shows that it takes the initial value "a'
  // then adds 4 * 633 to create a seed.
  // This seed is then repeatedly bit shifted right by dividing by 2
  // using an inefficient linear time loop.
  // The remainder (the bit that drops off) is the output.
  // This means that output sequence is simply the binary digits of "a" + 4 * 633.
  // To obtain the desired pattern we need the next highest binary number that has the
  // pattern "101010.....". For this data that number is 2730 - 4 * 633 = 198.
  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day25.txt").getLines().toSeq
    val magic = 198
    val result = Iterator.iterate(Cpu(data, Map("a" -> magic)))(_.step).take(100000).collect {
      case Cpu(_, _, _, Output(value)) => value
    }
    println(result.mkString)
