package AdventOfCode2022

object Day23:
  val adjacent = Seq(Point(-1, -1), Point(0, -1), Point(1, -1), Point(-1, 0), Point(1, 0), Point(-1, 1), Point(0, 1), Point(1, 1))
  val order = Seq(Point(0, -1), Point(0, 1), Point(-1, 0), Point(1, 0))
  val Seq(north, south, west, east) = order

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)

  case class State(elves: Set[Point], moves: Seq[Point], stuck: Boolean)

  def parse(input: Seq[String]): Set[Point] =
    val points = for y <- input.indices; x <- input.head.indices if input(y)(x) != '.' yield Point(x, y)
    points.toSet

  def step(state: State): State =
    val State(elves, moves, _) = state

    val proposals = elves.map(elf => elf -> propose(elves, moves, elf))
    val occurrences = proposals.toSeq.flatMap(_._2).groupMapReduce(identity)(_ => 1)(_ + _)
    val next = proposals.map { (elf, proposal) =>
      proposal.map(move => if occurrences(move) == 1 then move else elf).getOrElse(elf)
    }

    State(next, moves.tail :+ moves.head, elves == next)
  end step

  def propose(elves: Set[Point], moves: Seq[Point], elf: Point): Option[Point] =
    val checks = adjacent.map(_ + elf).map(elves.contains)
    val Seq(nw, n, ne, w, e, sw, s, se) = checks

    if checks.exists(identity) then
      moves.find {
        case `north` => !(nw || n || ne)
        case `south` => !(sw || s || se)
        case `west` => !(nw || w || sw)
        case `east` => !(ne || e || se)
      }
      .map(_ + elf)
    else None
  end propose

  def part1(input: Seq[String]): Int =
    val start = State(parse(input), order, false)
    val elves = Iterator.iterate(start)(step).drop(10).next.elves

    val (minX, maxX) = (elves.map(_.x).min, elves.map(_.x).max)
    val (minY, maxY) = (elves.map(_.y).min, elves.map(_.y).max)

    val points = for x <- minX to maxX; y <- minY to maxY yield Point(x, y)
    points.filterNot(elves.contains).size
  end part1

  def part2(input: Seq[String]): Int =
    val start = State(parse(input), order, false)
    Iterator.iterate(start)(step).indexWhere(_.stuck == true)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day23.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
