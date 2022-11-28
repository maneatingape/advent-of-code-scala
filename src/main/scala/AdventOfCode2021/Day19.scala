package AdventOfCode2021

import scala.util.control.NonLocalReturns.*

object Day19:
  case class Beacon(x: Int, y: Int, z: Int):
    def +(other: Beacon): Beacon = Beacon(x + other.x, y + other.y, z + other.z)
    def -(other: Beacon): Beacon = Beacon(x - other.x, y - other.y, z - other.z)
    def manhattan(other: Beacon): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
    def permutations: Seq[Beacon] = Seq(
      Beacon(x, y, z), Beacon(y, z, x), Beacon(z, x, y), Beacon(-x, z, y),
      Beacon(z, y, -x), Beacon(y, -x, z), Beacon(x, z, -y), Beacon(z, -y, x),
      Beacon(-y, x, z), Beacon(x, -z, y), Beacon(-z, y, x), Beacon(y, x, -z),
      Beacon(-x, -y, z), Beacon(-y, z, -x), Beacon(z, -x, -y), Beacon(-x, y, -z),
      Beacon(y, -z, -x), Beacon(-z, -x, y), Beacon(x, -y, -z), Beacon(-y, -z, x),
      Beacon(-z, x, -y), Beacon(-x, -z, -y), Beacon(-z, -y, -x), Beacon(-y, -x, -z))
  end Beacon

  case class Scanner(beacons: Seq[Beacon]):
    val deltas = (for first <- beacons; second <- beacons yield first - second).toSet
    def permutations: Seq[Scanner] = beacons.map(_.permutations).transpose.map(Scanner(_))

  def parse(input: String): Seq[Scanner] = input.split("\n\n").toSeq.map { block =>
    Scanner(block.trim.split("\n").tail.map(_.trim.split(",").map(_.toInt)).map(a => Beacon(a(0), a(1), a(2))).toSeq)
  }

  def findMatch(firstScanner: Scanner, candidate: Scanner): Option[(Scanner, Beacon)] = returning {
    for
      secondScanner <- candidate.permutations
      if secondScanner.deltas.intersect(firstScanner.deltas).size > 12 * 11
    do
      for
        firstBeacon <- firstScanner.beacons
        secondBeacon <- secondScanner.beacons
        if secondScanner.beacons.map(_ + firstBeacon - secondBeacon).toSet.intersect(firstScanner.beacons.toSet).size >= 12
      do throwReturn(Some((secondScanner, firstBeacon - secondBeacon)))
    None
  }

  def result(scanners: Seq[Scanner]): (Set[Beacon], Seq[Beacon]) =
    def helper(beacons: Set[Beacon], offsets: Seq[Beacon], todo: Map[Scanner, Beacon], remaining: Seq[Scanner]): (Set[Beacon], Seq[Beacon]) =
      if todo.isEmpty then (beacons, offsets) else
        val (current, currentOffset) = todo.head
        val nextBeacons = beacons ++ current.beacons.map(_ + currentOffset)
        val nextOffsets = offsets.appended(currentOffset)

        val (nextTodo, nextRemaining) = remaining.foldLeft((todo.tail, remaining)) { case ((todo, remaining), candidate) =>
          findMatch(current, candidate) match
            case Some((scanner, offset)) => (todo.updated(scanner, offset + currentOffset), remaining.filterNot(_ == candidate))
            case None => (todo, remaining)
        }

        helper(nextBeacons, nextOffsets, nextTodo, nextRemaining)
      end if
    end helper

    helper(Set(), Seq(), Map(scanners.head -> Beacon(0, 0, 0)), scanners.tail)
  end result

  def part1(input: String): Int =
    val (beacons, _) = result(parse(input))
    beacons.size

  def part2(input: String): Int =
    val (_, offsets) = result(parse(input))
    offsets.combinations(2).map(pair => pair.head.manhattan(pair.last)).max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day19.txt").mkString
    println(Day19.part1(data))
    println(Day19.part2(data))
