package AdventOfCode2021

object Day22:
  case class RebootStep(on: Boolean, cuboid: Cuboid)

  case class Cuboid(xs: Dimension, ys: Dimension, zs: Dimension):
    def volume: Long = xs.size * ys.size * zs.size

    def intersect(other: Cuboid): Option[Cuboid] =
      for
        nextXs <- xs.intersect(other.xs)
        nextYs <- ys.intersect(other.ys)
        nextZs <- zs.intersect(other.zs)
      yield Cuboid(nextXs, nextYs, nextZs)

    def split(other: Cuboid): Set[Cuboid] =
      for
        nextXs <- xs.split(other.xs)
        nextYs <- ys.split(other.ys)
        nextZs <- zs.split(other.zs)
      yield Cuboid(nextXs, nextYs, nextZs)
  end Cuboid

  case class Dimension(from: Int, to: Int):
    val (start, end) = if from <= to then (from, to) else (to, from)
    def size: Long = 1 + end - start
    def range: Range = start to end

    def intersect(other: Dimension): Option[Dimension] =
      val (s1, e1) = (start, end)
      val (s2, e2) = (other.start, other.end)
      if s2 <= s1 && e1 <= e2 then Some(this)
      else if s1 <= s2 && e2 <= e1 then Some(other)
      else if s2 <= s1 && s1 <= e2 then Some(Dimension(s1, e2))
      else if s1 <= s2 && s2 <= e1 then Some(Dimension(s2, e1))
      else None

    def split(other: Dimension): Set[Dimension] =
      val (s1, e1) = (start, end)
      val (s2, e2) = (other.start, other.end)
      if s2 <= s1 && e1 <= e2 then Set(this)
      else if s1 < s2 && e2 < e1 then Set(Dimension(s1, s2 - 1), Dimension(s2, e2), Dimension(e2 + 1, e1))
      else if s1 <= e2 && e2 < e1 then Set(Dimension(s1, e2), Dimension(e2 + 1, e1))
      else if s1 < s2 && s2 <= e1 then Set(Dimension(s1, s2 - 1), Dimension(s2, e2))
      else Set()
  end Dimension

  def parse(input: Seq[String]): List[RebootStep] =
    val regex = """(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".r
    input.map {
      case regex(state, fromX, toX, fromY, toY, fromZ, toZ) =>
        val xs = Dimension(fromX.toInt, toX.toInt)
        val ys = Dimension(fromY.toInt, toY.toInt)
        val zs = Dimension(fromZ.toInt, toZ.toInt)
        RebootStep(state == "on", Cuboid(xs, ys, zs))
    }
    .toList

  def part1(input: Seq[String]): Int =
    val range = Dimension(-50, 50)
    val bounds = Cuboid(range, range, range)
    val points = parse(input).foldLeft(Set.empty[(Int, Int, Int)]) { (points, rebootStep) =>
      rebootStep.cuboid.intersect(bounds).map { cuboid =>
        val next = for x <- cuboid.xs.range; y <- cuboid.ys.range; z <- cuboid.zs.range yield (x, y, z)
        if rebootStep.on then points ++ next else points -- next
      }
      .getOrElse(points)
    }
    points.size
  end part1

  def part2(input: Seq[String]): Long =
    def helper(todo: List[RebootStep], reactor: Set[Cuboid]): Set[Cuboid] = todo match
      case Nil => reactor
      case RebootStep(true, head) :: tail =>
        reactor.find(_.intersect(head).isDefined) match
          case None => helper(tail, reactor + head)
          case Some(collision) =>
            val intersect = collision.intersect(head).get
            val remaining = (head.split(intersect) - intersect).map(RebootStep(true, _))
            helper(remaining.toList ++ tail, reactor)
      case RebootStep(false, head) :: tail =>
        val nextReactor = reactor.flatMap { cuboid =>
          cuboid.intersect(head) match
            case None => Set(cuboid)
            case Some(intersect) => cuboid.split(intersect) - intersect
        }
        helper(tail, nextReactor)
    end helper

    helper(parse(input), Set()).toSeq.map(_.volume).sum
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day22.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
