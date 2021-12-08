package AdventOfCode2021

object Day08:
  case class Entry(patterns: Seq[String], digits: Seq[String])

  // All possible segments
  val segments = Seq('a', 'b', 'c', 'd', 'e', 'f', 'g')
  // Map sorted non-scrambled segment pattern to digit
  val segmentsToDigit = Map("abcefg" -> 0, "cf" -> 1, "acdeg" -> 2, "acdfg" -> 3, "bcdf" -> 4, "abdfg" -> 5, "abdefg" -> 6, "acf" -> 7, "abcdefg" -> 8, "abcdfg" -> 9)

  def parseEntry(entry: String): Entry =
    val Array(patterns, digits) = entry.split(" \\| ")
    Entry(patterns.split(" "), digits.split(" "))

  def isKnownDigit(pattern: String): Boolean = pattern.length match
    case 2 | 3 | 4 | 7 => true
    case _ => false

  def unscrambleEntry(entry: Entry): Int =
    // One is the only pattern 2 characters long and Seven the only pattern 3 characters long
    def fromKnownLength(x: Int) = entry.patterns.filter(_.length == x).flatten.toSet
    val setCF = fromKnownLength(2)
    val setACF = fromKnownLength(3)

    // Considering only the digits 0, 2, 3, 5, 6 and 9 then segments a and g occur 6 times, segments d and f 5 times,
    // segments b and c 4 times and segment e 3 times.
    val unknownPatterns = entry.patterns.filterNot(isKnownDigit)
    val unknownOccurrences = unknownPatterns.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
    def fromUnknownOccurrences(x: Int) = segments.filter(unknownOccurrences(_) == x).toSet

    val setE = fromUnknownOccurrences(3)
    val setBC = fromUnknownOccurrences(4)
    val setAG = fromUnknownOccurrences(6)
    val setDF = fromUnknownOccurrences(5)

    val setA = setACF.diff(setCF)
    val setB = setBC.diff(setCF)
    val setC = setBC.diff(setB)
    val setD = setDF.diff(setCF)
    val setF = setDF.diff(setD)
    val setG = setAG.diff(setA)

    val unscramble = Seq(setA, setB, setC, setD, setE, setF, setG).zip(segments).map((k, v) => (k.head, v)).toMap

    entry.digits.map(scrambled => segmentsToDigit(scrambled.map(unscramble).sorted)).mkString.toInt
  end unscrambleEntry

  def part1(input: Seq[String]): Int = input.map(parseEntry).map(_.digits.count(isKnownDigit)).sum

  def part2(input: Seq[String]): Int = input.map(parseEntry).map(unscrambleEntry).sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day08.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
