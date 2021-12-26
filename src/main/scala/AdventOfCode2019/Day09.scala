package AdventOfCode2019

object Day09:
  object IntCode:
    val powers = Map(1 -> 100, 2 -> 1000, 3 -> 10000)

    sealed trait State
    case object Initial extends State
    case object Running extends State
    case object Halted extends State
    case class Output(value: Long) extends State

    def apply(input: Seq[Long]): IntCode =
      val memory = input.zipWithIndex.map((value, index) => index.toLong -> value)
      IntCode(0, 0, memory.toMap.withDefaultValue(0), Seq(), Initial)
  end IntCode

  case class IntCode(ip: Long, relativeBase: Long, memory: Map[Long, Long], input: Seq[Long], result: IntCode.State):
    import IntCode._

    private def next: IntCode = memory(ip) % 100 match
      case 1 => copy(ip = ip + 4, memory = write(3, read(1) + read(2)), result = Running)                   // Add
      case 2 => copy(ip = ip + 4, memory = write(3, read(1) * read(2)), result = Running)                   // Multiply
      case 3 => copy(ip = ip + 2, memory = write(1, input.head), input = input.tail, result = Running)      // Read
      case 4 => copy(ip = ip + 2, result = Output(read(1)))                                                 // Write
      case 5 => copy(ip = if read(1) != 0 then read(2) else ip + 3, result = Running)                       // Jump if true
      case 6 => copy(ip = if read(1) == 0 then read(2) else ip + 3, result = Running)                       // Jump if false
      case 7 => copy(ip = ip + 4, memory = write(3, if read(1) < read(2) then 1 else 0), result = Running)  // Less than
      case 8 => copy(ip = ip + 4, memory = write(3, if read(1) == read(2) then 1 else 0), result = Running) // Equals
      case 9 => copy(ip = ip + 2, relativeBase = relativeBase + read(1), result = Running)                  // Relative base
      case 99 => copy(result = Halted)                                                                      // Halt

    private def read(offset: Int): Long = (memory(ip) / powers(offset)) % 10 match
      case 0 => memory(memory(ip + offset))
      case 1 => memory(ip + offset)
      case 2 => memory(relativeBase + memory(ip + offset))

    private def write(offset: Int, value: Long): Map[Long, Long] = (memory(ip) / powers(offset)) % 10 match
      case 0 => memory.updated(memory(ip + offset), value)
      case 2 => memory.updated(relativeBase + memory(ip + offset), value)

    def withInput(next: Long*): IntCode = copy(input = input.appendedAll(next))

    def nextOutput: IntCode = Iterator.iterate(next)(_.next).dropWhile(_.result == Running).next()

    def lastOutput: Long =
      val output = Iterator.iterate(this)(_.nextOutput).takeWhile(_.result != Halted)
      output.toSeq.map(_.result).collect { case Output(value) => value }.last
  end IntCode

  def part1(memory: Seq[Long]): Long = IntCode(memory).withInput(1).lastOutput

  def part2(memory: Seq[Long]): Long = IntCode(memory).withInput(2).lastOutput

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day09.txt").mkString.trim.split(",").map(_.toLong).toSeq
    println(part1(data))
    println(part2(data))
