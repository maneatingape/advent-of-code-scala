package AdventOfCode2018

object Day20:
  case class Point(x: Int, y: Int):
    def move(direction: Char): Point = direction match
      case 'N' => Point(x, y - 1)
      case 'S' => Point(x, y + 1)
      case 'E' => Point(x - 1, y)
      case 'W' => Point(x + 1, y)

  case class Path(point: Point, max: Int, visited: Map[Point, Int]):
    def move(direction: Char): Path =
      val nextPoint = point.move(direction)
      val (nextMax, nextVisited) = if visited.contains(nextPoint) then (max, visited) else
        val distance = visited(point) + 1
        (max.max(distance), visited + (nextPoint -> distance))
      Path(nextPoint, nextMax, nextVisited)

  sealed trait Node
  case class Parallel(nodes: Seq[Node]) extends Node
  case class Serial(nodes: Seq[Node]) extends Node
  case class Leaf(segment: String) extends Node

  def parse(current: String): Node =
    val parallel = scanParallel(current)
    val serial = scanSerial(current)
    if parallel.size > 2 then Parallel(split(current, parallel).map(parse))
    else if serial.size > 2 then Serial(split(current, serial).map(parse))
    else Leaf(current)

  def split(string: String, indices: Seq[Int]): Seq[String] =
    indices.sliding(2).toSeq.map { case Seq(from, until) => string.slice(from + 1, until) }

  def scanParallel(string: String): Seq[Int] =
    val (indices, _) = string.zipWithIndex.foldLeft((Seq(-1), 0)) { case ((indices, level), (next, index)) =>
      next match
        case '|' if level == 0 => (indices.appended(index), 0)
        case '(' => (indices, level + 1)
        case ')' => (indices, level - 1)
        case _ => (indices, level)
    }
    indices.appended(string.length)

  def scanSerial(string: String): Seq[Int] =
    val (indices, _) = string.zipWithIndex.foldLeft((Seq(-1), 0)) { case ((indices, level), (next, index)) =>
      next match
        case '(' if level == 0 => (indices.appended(index), level + 1)
        case ')' if level == 1 => (indices.appended(index), level - 1)
        case '(' => (indices, level + 1)
        case ')' => (indices, level - 1)
        case _ => (indices, level)
    }
    if indices.last < string.length - 1 then indices.appended(string.length) else indices

  def walk(input: String): Seq[Path] =
    def helper(node: Node, paths: Seq[Path]): Seq[Path] =
      val next = node match
        case Leaf(segment) => paths.map(path => segment.foldLeft(path)((path, next) => path.move(next)))
        case Serial(nodes) => nodes.foldLeft(paths)((paths, next) => helper(next, paths))
        case Parallel(nodes) => nodes.flatMap(next => helper(next, paths))
      // Deduplicate to prevent exponential explosion
      next.groupBy(_.point).values.map(_.maxBy(_.max)).toSeq

    val node = parse(input.tail.init)
    val start = Path(Point(0, 0), 0, Map(Point(0, 0) -> 0))
    helper(node, Seq(start))
  end walk

  def part1(input: String): Int = walk(input).map(_.max).max

  def part2(input: String, threshold: Int): Int =
    walk(input).flatMap(_.visited.toSeq).groupMap(_._1)(_._2).values.count(_.min >= threshold)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day20.txt").mkString.trim
    println(part1(data))
    println(part2(data, 1000))
