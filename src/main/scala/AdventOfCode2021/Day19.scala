package AdventOfCode2021

class Day19(input: String):
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
    def permutations: Seq[Scanner] = beacons.map(_.permutations).transpose.map(Scanner(_))

  def parse(input: String): Seq[Scanner] = input.split("\n\n").zipWithIndex.map { (block, index) =>
    Scanner(block.trim.split("\n").tail.map(_.trim.split(",").map(_.toInt)).map(a => Beacon(a(0), a(1), a(2))))
  }

  def findMatch(beacons: Set[Beacon], candidate: Scanner): Option[(Scanner, Beacon)] =
    val list = for
      firstBeacon <- beacons
      secondScanner <- candidate.permutations
      secondBeacon <- secondScanner.beacons
      if secondScanner.beacons.map(_ + firstBeacon - secondBeacon).toSet.intersect(beacons).size >= 12
    do return Some((secondScanner, firstBeacon - secondBeacon))
    None
  end findMatch

  lazy val result: (Int, Int) = {
    def helper(beacons: Set[Beacon], offsets: Seq[Beacon], todo: Map[Scanner, Beacon], remaining: Seq[Scanner]): (Set[Beacon], Seq[Beacon]) =
      println(beacons.size + " " + todo.size + " " + remaining.size)
      if todo.isEmpty then (beacons, offsets) else
        val (current, currentOffset) = todo.head
        val nextBeacons = beacons ++ current.beacons.map(_ + currentOffset)
        val nextOffsets = offsets.appended(currentOffset)

        val (nextTodo, nextRemaining) = remaining.foldLeft((todo.tail, remaining)) { case ((todo, remaining), candidate) =>
          findMatch(current.beacons.toSet, candidate) match
            case Some((scanner, offset)) => (todo.updated(scanner, offset + currentOffset), remaining.filterNot(_ == candidate))
            case None => (todo, remaining)
        }

        helper(nextBeacons, nextOffsets, nextTodo, nextRemaining)
      end if
    end helper

    val scanners = parse(input)
    val (beacons, offsets) = helper(Set(), Seq(), Map(scanners.head -> Beacon(0, 0, 0)), scanners.tail)
    (beacons.size, offsets.combinations(2).toSeq.map { case Seq(first, second) => first.manhattan(second) }.max)
  }

  def part1: Int = result._1
  def part2: Int = result._2

object Day19:
  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day19.txt").mkString
    val day19 = Day19(data)
    println(day19.part1)
    println(day19.part2)
