package AdventOfCode2022

object Day07:
  type Path = List[String]

  def build(input: Seq[String]): Seq[Int] =
    val root = List("/")
    val initial = Map(root -> 0)
    val (_, sizes) = input.foldLeft((root, initial)) { case ((path, sizes), line) =>
      line match
        case "$ ls" => (path, update(path, sizes, -sizes(path)))
        case "$ cd \\" => (List(path.last), sizes)
        case "$ cd .." => (path.tail, sizes)
        case s"$$ cd $name" => (name :: path, sizes.updated(name :: path, 0))
        case s"dir $name" => (path, sizes)
        case s"$size $name" => (path, update(path, sizes, size.toInt))
    }
    sizes.values.toSeq

  def update(path: Path, sizes: Map[Path, Int], delta: Int): Map[Path, Int] = path match
    case Nil => sizes
    case _ :: tail => update(tail, sizes, delta).updated(path, sizes(path) + delta)

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
