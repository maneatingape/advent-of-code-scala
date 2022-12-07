package AdventOfCode2022

object Day07:
  class Dir() {
    val files = scala.collection.mutable.Map[String, Int]()
    val dirs = scala.collection.mutable.Map[String, Dir]()
  }

  def build(input: Seq[String]): Dir = input
    .foldLeft(List(Dir())) { (path, line) =>
      line.split(" ") match {
        case Array("$", "ls") => path
        case Array("$", "cd", "\\") => List(path.last)
        case Array("$", "cd", "..") => path.tail
        case Array("$", "cd", name) => path.head.dirs.getOrElseUpdate(name, Dir()) :: path
        case Array("dir", name) => path
        case Array(size, name) =>
          path.head.files.update(name, size.toInt)
          path
      }
    }
    .last

  def visit(root: Dir): List[Int] =
    var list = List.empty[Int]

    def helper(current: Dir): Int =
      val size = current.files.values.sum + current.dirs.values.map(helper).sum
      list = size :: list
      size

    helper(root)
    list
  end visit

  def part1(input: Seq[String]): Int =
    val sizes = visit(build(input))
    sizes.filter(_ <= 100000).sum

  def part2(input: Seq[String]): Int =
    val sizes = visit(build(input))
    val needed = 30000000 - (70000000 - sizes.head)
    sizes.filter(_ >= needed).min

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day07.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
