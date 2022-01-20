package AdventOfCode2016

object Day17:
  val md5 = java.security.MessageDigest.getInstance("MD5")
  val directions = Seq('U' -> Point(0, -1), 'D' -> Point(0, 1), 'L' -> Point(-1, 0), 'R' -> Point(1, 0))

  case class Point(x: Int, y: Int):
    def valid: Boolean = 0 <= x && x < 4 && 0 <= y && y < 4
    def delta(other: Point): Point = Point(x + other.x, y + other.y)

  case class State(location: Point, path: String):
    def hash(string: String): String = md5.digest(string.getBytes).take(2).map(_.formatted("%02x")).mkString
    def neighbours(passcode: String): Set[State] = directions
      .map((next, move) => State(location.delta(move), path + next))
      .zip(hash(passcode + path))
      .filter((state, char) => state.location.valid && char > 'a')
      .map(_._1)
      .toSet

  def dfs(passcode: String): (String, Int) =
    def helper(state: State): Set[String] =
      if state.location == Point(3, 3) then Set(state.path)
      else state.neighbours(passcode).flatMap(helper)

    val paths = helper(State(Point(0, 0), ""))
    (paths.minBy(_.size), paths.map(_.size).max)
  end dfs

  def part1(input: String): String = dfs(input)._1

  def part2(input: String): Int = dfs(input)._2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day17.txt").mkString.trim
    println(part1(data))
    println(part2(data))
