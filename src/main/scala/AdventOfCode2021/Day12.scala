package AdventOfCode2021

object Day12:
  case class Cave(name: String, small: Boolean, paths: List[String])

  def parse(input: Seq[String]): Map[String, Cave] =
    def createCave(name: String) = Cave(name, name != "start" && name != "end" && name.head.isLower, Nil)
    def helper(input: Seq[String], map: Map[String, Cave]): Map[String, Cave] = input match
      case Nil => map
      case head :: tail =>
        val Array(first, second) = head.split("-")
        val start = map.getOrElse(first, createCave(first))
        val end = map.getOrElse(second, createCave(second))
        val nextStart = start.copy(paths = second :: start.paths)
        val nextEnd = end.copy(paths = first :: end.paths)
        val nextMap = map.updated(first, nextStart).updated(second, nextEnd)
        helper(tail, nextMap)
    helper(input, Map())

  def part1(input: Seq[String]): Int =
    val caverns = parse(input)
    val paths = collection.mutable.ListBuffer[List[String]]()

    def descend(path: List[String]): Unit =
      val (head :: tail) = path
      if head == "end" then paths += path.reverse
      else caverns(head).paths.filterNot(next => next == "start" || (caverns(next).small && path.contains(next))).foreach(next => descend(next :: path))

    descend(List("start"))
    paths.length
  end part1

  def part2(input: Seq[String]): Int =
    val caverns = parse(input)
    val paths = collection.mutable.ListBuffer[List[String]]()

    def rule(path: List[String]): Boolean =
      val small = path.filter(caverns(_).small)
      val occurrences = small.groupMapReduce(identity)(_ => 1)(_ + _)
      occurrences.forall((k, v) => v <= 2) && occurrences.count((k, v) => v == 2) <= 1

    def descend(path: List[String]): Unit =
      val (head :: tail) = path
      if head == "end" then paths += path.reverse
      else caverns(head).paths.filterNot(next => next == "start" || (caverns(next).small && !rule(next :: path))).foreach(next => descend(next :: path))

    descend(List("start"))
    paths.length
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day12.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
