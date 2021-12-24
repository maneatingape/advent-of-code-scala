package AdventOfCode2021

object Day24:
  def add(x: Long, y: Long) = x + y
  def mul(x: Long, y: Long) = x * y
  def div(x: Long, y: Long) = x / y
  def mod(x: Long, y: Long) = x % y
  def eql(x: Long, y: Long) = if x == y then 1L else 0L

  val indexes = Map("w" -> 0, "x" -> 1, "y" -> 2, "z" -> 3)
  val execute = Map("add" -> add, "mul" -> mul, "div" -> div, "mod" -> mod, "eql" -> eql)

  def execute(instructions: Seq[String], order: Boolean): String =
    val visited = collection.mutable.Set[(Seq[Long], Int)]()
    var finished = Option.empty[Seq[Int]]

    def helper(state: Seq[Long], instructionIndex: Int, monadIndex: Int, path: Seq[Int]): Unit =
      if finished.isDefined then return ()

      if instructionIndex >= instructions.size then
        if state(3) == 0 then finished = Some(path)
        return ()
      end if

      val decoded = instructions(instructionIndex).split(" ")
      val dest = indexes(decoded(1))

      if decoded.length == 2 then
        for
          nextW <- if order then (9 to 1 by - 1) else (1 to 9)
        do
          val nextState = state.updated(0, nextW.toLong)
          if !visited.contains((nextState, monadIndex)) then
            visited.add((nextState, monadIndex))
            helper(nextState, instructionIndex + 1, monadIndex + 1, path.appended(nextW))
      else if indexes.contains(decoded(2)) then
        val op = execute(decoded(0))
        val src = indexes(decoded(2))
        val nextState = state.updated(dest, op(state(dest), state(src)))
        helper(nextState, instructionIndex + 1, monadIndex, path)
      else
        val op = execute(decoded(0))
        val const = decoded(2).toLong
        val nextState = state.updated(dest, op(state(dest), const))
        helper(nextState, instructionIndex + 1, monadIndex, path)
      end if
    end helper

    helper(Seq(0, 0, 0, 0), 0, 0, Seq())
    finished.get.mkString
  end execute

  def part1(input: Seq[String]): String = execute(input, true)

  def part2(input: Seq[String]): String = execute(input, false)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day24.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
