package AdventOfCode2020

object Day14:
  val maskRegex = "mask = ([X10]{36})".r
  val memRegex = "mem\\[(\\d+)\\] = (\\d+)".r

  type Memory = Map[Long, Long]

  case class Version1(pattern: String):
    val on = binary(pattern.map(c => if c == '1' then 1 else 0))
    val off = binary(pattern.map(c => if c == '0' then 0 else 1))
    def binary(seq: Seq[Int]): Long = java.lang.Long.parseLong(seq.mkString, 2)
    def modify(value: Long): Long = (value | on) & off

  case class Version2(pattern: String):
    val version1 = {
      def helper(todo: List[Char], path: List[Char]): Seq[Version1] = todo match
        case Nil => Seq(Version1(path.reverse.mkString))
        case ('0' :: tail) => helper(tail, 'X' :: path)
        case ('1' :: tail) => helper(tail, '1' :: path)
        case ('X' :: tail) => helper(tail, '0' :: path) ++ helper(tail, '1' :: path)
      helper(pattern.toList, Nil)
    }

  def part1(input: Seq[String]): Long =
    val (_, memory) = input.foldLeft[(Version1, Memory)]((Version1("X"), Map())) {
      case ((mask, memory), maskRegex(pattern)) => Version1(pattern) -> memory
      case ((mask, memory), memRegex(address, value)) => mask -> memory.updated(address.toLong, mask.modify(value.toLong))
    }
    memory.values.sum

  def part2(input: Seq[String]): Long =
    val (_, memory) = input.foldLeft[(Version2, Memory)]((Version2("X"), Map())) {
      case ((mask, memory), maskRegex(pattern)) => Version2(pattern) -> memory
      case ((mask, memory), memRegex(address, value)) =>
        val newMemory = mask.version1.foldLeft(memory) { (memory, mask) =>
          memory.updated(mask.modify(address.toLong), value.toLong)
        }
        mask -> newMemory
    }
    memory.values.sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day14.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
