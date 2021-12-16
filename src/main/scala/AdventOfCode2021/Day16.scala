package AdventOfCode2021

object Day16:
  val hexToBinary = Map('0' -> "0000", '1' -> "0001", '2' -> "0010", '3' -> "0011",
                        '4' -> "0100", '5' -> "0101", '6' -> "0110", '7' -> "0111",
                        '8' -> "1000", '9' -> "1001", 'A' -> "1010", 'B' -> "1011",
                        'C' -> "1100", 'D' -> "1101", 'E' -> "1110", 'F' -> "1111")

  def parse(hex: String): Seq[Int] = hex.flatMap(hexToBinary).toSeq.map(_.asDigit)
  def binary(digits: Seq[Int]): Long = java.lang.Long.parseLong(digits.mkString, 2)

  sealed trait Packet
  case class Literal(version: Long, typeId: Long, value: Long) extends Packet
  case class Operator(version: Long, typeId: Long, packets: List[Packet]) extends Packet

  def decode(input: Seq[Int]): (Int, Packet) =
    val version = binary(input.take(3))
    val typeId = binary(input.drop(3).take(3))
    typeId match
      case 4 => decodeLiteral(version, typeId, input)
      case _ =>
        val lengthTypeId = input.drop(6).head
        if lengthTypeId == 1 then decodeOperatorBitLength(version, typeId, input)
        else decodeOperatorSubPackets(version, typeId, input)

  def decodeLiteral(version: Long, typeId: Long, input: Seq[Int]): (Int, Packet) =
    val (_, groups, read) = Iterator.iterate((1, Seq.empty[Int], 6)) { case (prefix, groups, read) =>
      val nextPrefix = input.drop(read).head
      val nextGroup = groups ++ input.drop(read + 1).take(4)
      val nextRead = read + 5
      (nextPrefix, nextGroup, nextRead)
    }
    .dropWhile(_._1 == 1).next()
    (read, Literal(version, typeId, binary(groups)))

  def decodeOperatorBitLength(version: Long, typeId: Long, input: Seq[Int]): (Int, Packet) =
    val subPackets = binary(input.drop(7).take(11)).toInt
    val (read, packets) = Iterator.iterate((18, List.empty[Packet]))(decodeNextPacket(input)).drop(subPackets).next()
    (read, Operator(version, typeId, packets.reverse))

  def decodeOperatorSubPackets(version: Long, typeId: Long, input: Seq[Int]): (Int, Packet) =
    val subLength = binary(input.drop(7).take(15)) + 22
    val (read, packets) = Iterator.iterate((22, List.empty[Packet]))(decodeNextPacket(input)).dropWhile(_._1 < subLength).next()
    (read, Operator(version, typeId, packets.reverse))

  def decodeNextPacket(input: Seq[Int])(read: Int, packets: List[Packet]): (Int, List[Packet]) =
    val (nextRead, packet) = decode(input.drop(read))
    (read + nextRead, packet :: packets)

  def versionSum(packet: Packet): Long = packet match
    case Literal(version, _, _) => version
    case Operator(version, _, packets) => version + packets.map(versionSum).sum

  def expressionValue(packet: Packet): Long = packet match
    case Literal(_, _, value) => value
    case Operator(_, 0, packets) => packets.map(expressionValue).sum
    case Operator(_, 1, packets) => packets.map(expressionValue).product
    case Operator(_, 2, packets) => packets.map(expressionValue).min
    case Operator(_, 3, packets) => packets.map(expressionValue).max
    case Operator(_, 5, packets) =>
      val List(first, second) = packets.map(expressionValue)
      if first > second then 1L else 0L
    case Operator(_, 6, packets) =>
      val List(first, second) = packets.map(expressionValue)
      if first < second then 1L else 0L
    case Operator(_, 7, packets) =>
      val List(first, second) = packets.map(expressionValue)
      if first == second then 1L else 0L

  def part1(input: Seq[String]): Long = input.map(parse).map(decode).map(_._2).map(versionSum).sum

  def part2(input: Seq[String]): Long = input.map(parse).map(decode).map(_._2).map(expressionValue).sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day16.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
