package AdventOfCode2017

object Day09:
  case class State(groupDepth: Int, groupScore: Int, garbage: Boolean, garbageScore: Int, ignoreNext: Boolean):
    def step(next: Char): State = next match
      case _ if ignoreNext => copy(ignoreNext = false)
      case '!' => copy(ignoreNext = true)
      case '>' => copy(garbage = false)
      case _ if garbage => copy(garbageScore = garbageScore + 1)
      case '{' => copy(groupDepth = groupDepth + 1, groupScore = groupScore + groupDepth)
      case '}' => copy(groupDepth = groupDepth - 1)
      case '<' => copy(garbage = true)
      case _ => this

  def stream(input: String): State =
    val initial = State(1, 0, false, 0, false)
    input.foldLeft(initial)((state, next) => state.step(next))

  def part1(input: String): Int = stream(input).groupScore

  def part2(input: String): Int = stream(input).garbageScore

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day09.txt").mkString.trim
    println(part1(data))
    println(part2(data))
