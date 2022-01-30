package AdventOfCode2021

object Day24:
  sealed trait Instruction
  case class Inp(ws: Range) extends Instruction
  case class Alu(op: (Long, Long) => Long, dest: Int, src: Seq[Long] => Long) extends Instruction

  def add(x: Long, y: Long): Long = x + y
  def mul(x: Long, y: Long): Long = x * y
  def div(x: Long, y: Long): Long = x / y
  def mod(x: Long, y: Long): Long = x % y
  def eql(x: Long, y: Long): Long = if x == y then 1L else 0L
  def register(i: Int)(input: Seq[Long]): Long = input(i)
  def constant(i: Int)(input: Seq[Long]): Long = i

  def parse(input: IndexedSeq[String], smallest: Boolean): IndexedSeq[Instruction] =
    val indexes = Map("w" -> 0, "x" -> 1, "y" -> 2, "z" -> 3)
    val execute = Map("add" -> add, "mul" -> mul, "div" -> div, "mod" -> mod, "eql" -> eql)
    val ws = if smallest then 1 to 9 else 9 to 1 by - 1
    input.map(_.split(" ")).map {
      case Array(_, _) => Inp(ws)
      case Array(op, dest, src) =>
        Alu(execute(op), indexes(dest), indexes.get(src).map(register).getOrElse(constant(src.toInt)))
    }

  def execute(instructions: Seq[Instruction]): String =
    val visited = collection.mutable.Set[(Seq[Long], Int)]()
    var finished = ""

    def helper(state: Seq[Long], instructionIndex: Int, path: Seq[Int]): Unit =
      if finished.nonEmpty then return ()

      if instructionIndex >= instructions.size then
        if state(3) == 0 then finished = path.mkString
        return ()
      end if

      instructions(instructionIndex) match
        case Alu(op, dest, src) =>
          val nextState = state.updated(dest, op(state(dest), src(state)))
          helper(nextState, instructionIndex + 1, path)
        case Inp(ws) => ws.foreach { nextW =>
          val nextState = state.updated(0, nextW.toLong)
          if !visited.contains(nextState -> instructionIndex) then
            visited.add(nextState -> instructionIndex)
            helper(nextState, instructionIndex + 1, path.appended(nextW))
        }
    end helper

    helper(Seq(0, 0, 0, 0), 0, Seq())
    finished
  end execute

  def part1(input: IndexedSeq[String]): String = execute(parse(input, false))

  def part2(input: IndexedSeq[String]): String = execute(parse(input, true))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day24.txt").getLines().toIndexedSeq
    println(part1(data))
    println(part2(data))
