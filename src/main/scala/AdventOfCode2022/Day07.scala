package AdventOfCode2022

object Day07:
  def build(input: Seq[String]): Seq[Int] =
    val root = List("/")
    val initial = Map(root -> 0)
    val (_, sizes) = input.foldLeft((root, initial)) { case ((path, sizes), line) =>
      line.split(" ") match
        case Array("$", "ls") => (path, update(path, sizes, -sizes(path)))
        case Array("$", "cd", "\\") => (List(path.last), sizes)
        case Array("$", "cd", "..") => (path.tail, sizes)
        case Array("$", "cd", name) => (name :: path, sizes.updated(name :: path, 0))
        case Array("dir", name) => (path, sizes)
        case Array(size, name) => (path, update(path, sizes, size.toInt))
    }
    sizes.values.toSeq

  def update(path: List[String], sizes: Map[List[String], Int], delta: Int): Map[List[String], Int] = path match
    case Nil => sizes
    case head :: tail => update(tail, sizes, delta).updated(path, sizes(path) + delta)

  def part1(input: Seq[String]): Int =
    val sizes = build(input)
    sizes.filter(_ <= 100000).sum

  def part2(input: Seq[String]): Int =
    val sizes = build(input)
    val needed = 30000000 - (70000000 - sizes.max)
    sizes.filter(_ >= needed).min

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day07.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
