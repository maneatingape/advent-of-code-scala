package AdventOfCode2019

object Day11:
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

  enum Direction:
    case Up, Down, Left, Right

  case class Point(x: Int, y: Int):
    def updated(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  import IntCode._
  import Direction._

  val clockwise = Map(Up -> Right, Right -> Down, Down -> Left, Left -> Up)
  val antiClockwise = Map(Up -> Left, Left -> Down, Down -> Right, Right -> Up)
  val deltas = Map(Up -> (0, -1), Down -> (0, 1), Left -> (-1, 0), Right -> (1, 0))

  def paint(intCode: IntCode, direction: Direction, position: Point, panels: Map[Point, Long]): Map[Point, Long] =
    val first = intCode.withInput(panels(position)).nextOutput
    first.result match
      case Halted => panels
      case Output(color) =>
        val second = first.nextOutput
        val Output(turn) = second.result

        val nextDirection = if turn == 1 then clockwise(direction) else antiClockwise(direction)
        val nextPosition = position.updated.tupled(deltas(nextDirection))
        val nextPanels = panels.updated(position, color)

        paint(second, nextDirection, nextPosition, nextPanels)
  end paint

  def part1(memory: Seq[Long]): Long = paint(IntCode(memory), Up, Point(0, 0), Map().withDefaultValue(0L)).size

  def part2(memory: Seq[Long]): Map[Point, Long] = paint(IntCode(memory), Up, Point(0, 0), Map(Point(0, 0) -> 1L).withDefaultValue(0L))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day11.txt").mkString.trim.split(",").map(_.toLong).toSeq
    println(part1(data))

    val panels = part2(data)
    val minX = panels.keys.map(_.x).min
    val maxX = panels.keys.map(_.x).max
    val minY = panels.keys.map(_.y).min
    val maxY = panels.keys.map(_.y).max

    for y <- minY to maxY do
      println()
      for x <- minX to maxX do
        print(if panels(Point(x, y)) == 1 then '#' else ' ')
  end main
