package AdventOfCode2016

object Day04:
  case class Room(name: String, id: Int, checksum: String)

  def parse(input: Seq[String]): Seq[Room] =
    val regex = """([\w\-]+)-(\d+)\[(\w{5})\]""".r
    input.map { case regex(name, id, checksum) => Room(name, id.toInt, checksum) }

  def real(room: Room): Boolean =
    val occurrences = room.name.filterNot(_ == '-').groupMapReduce(identity)(_ => 1)(_ + _)
    room.checksum == occurrences.toSeq.map((a,b) => (-b,a)).sorted.take(5).map(_._2).mkString

  def decode(room: Room): Room =
    room.copy(name = room.name.map(c => if c == '-' then ' ' else ('a' + (c - 'a' + room.id) % 26).toChar))

  def part1(input: Seq[String]): Int = parse(input).filter(real).map(_.id).sum

  def part2(input: Seq[String]): Int =
    parse(input).filter(real).map(decode).find(_.name == "northpole object storage").get.id

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day04.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
