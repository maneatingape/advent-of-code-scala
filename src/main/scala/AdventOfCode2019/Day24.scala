package AdventOfCode2019

object Day24:
  case class Point(x: Int, y: Int, z: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy, z)
    def orthogonal: Seq[Point] = Seq((1, 0), (-1, 0), (0, 1), (0, -1)).map(delta)

  def neighbours2D(point: Point): Seq[Point] =
    point.orthogonal.filter(p => p.x >= 0 && p.x < 5 && p.y >= 0 && p.y < 5)

  def neighbours3D(point: Point): Seq[Point] = point.orthogonal.flatMap {
    case Point(-1, y, z) => Set(Point(1, 2, z - 1))
    case Point(5, y, z) => Set(Point(3, 2, z - 1))
    case Point(x, -1, z) => Set(Point(2, 1, z - 1))
    case Point(x, 5, z) => Set(Point(2, 3, z - 1))
    case Point(2, 2, z) => point match
      case Point(2, 1, z) => Set.tabulate(5)(x => Point(x, 0, z + 1))
      case Point(2, 3, z) => Set.tabulate(5)(x => Point(x, 4, z + 1))
      case Point(1, 2, z) => Set.tabulate(5)(y => Point(0, y, z + 1))
      case Point(3, 2, z) => Set.tabulate(5)(y => Point(4, y, z + 1))
    case other => Set(other)
  }

  def parse(input: Seq[String]): Set[Point] =
    Set.tabulate(5, 5)((x, y) => Option.when(input(y)(x) == '#')(Point(x, y, 0))).flatten.flatten

  def step(grid: Set[Point], neighbours: Point => Seq[Point]): Set[Point] =
    val candidates = grid ++ grid.flatMap(neighbours)
    candidates.flatMap { point =>
      (grid.contains(point), neighbours(point).count(grid.contains)) match
        case (_, 1) => Some(point)
        case (false, 2) => Some(point)
        case _ => None
    }

  def part1(input: Seq[String]): Int =
    def helper(grid: Set[Point], previous: Set[Set[Point]]): Int =
      val next = step(grid, neighbours2D)
      if previous.contains(next) then next.map(point => 1 << (point.x + 5 * point.y)).sum
      else helper(next, previous + next)

    helper(parse(input), Set())
  end part1

  def part2(input: Seq[String], minutes: Int): Int =
    Iterator.iterate(parse(input))(step(_, neighbours3D)).drop(minutes).next().size

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day24.txt").getLines().toSeq
    println(part1(data))
    println(part2(data, 200))
