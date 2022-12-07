package AdventOfCode2022

object Day07:
  def build(input: Seq[String]): Seq[Int] =
    val root = List.empty[String]
    val initial = (root, Map(root -> 0))
    val (_, sizes) = input.foldLeft(initial) { case ((path, sizes), line) =>
      line match
        case "$ cd .." => (path.tail, sizes)
        case s"$$ cd $name" => (name :: path, sizes.updated(name :: path, 0))
        case s"$size $name" if size.head.isDigit =>
          (path, path.tails.foldLeft(sizes)((acc, next) => acc.updated(next, sizes(next) + size.toInt)))
        case _ => (path, sizes)
    }
    sizes.values.toSeq

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
