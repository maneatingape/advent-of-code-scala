package AdventOfCode2022

object Day02:
  def parse(input: Seq[String]): Seq[(String, String)] = input.map { line =>
    val Array(elf, you) = line.split(" ")
    (elf, you)
  }

  // A = Rock, B = Paper, C = Scissors
  // X = Rock (1), Y = Paper (2), Z = Scissors (3)
  def part1(input: Seq[String]): Int = parse(input).map {
    case ("A", "X") => 3 + 1
    case ("A", "Y") => 6 + 2
    case ("A", "Z") => 0 + 3
    case ("B", "X") => 0 + 1
    case ("B", "Y") => 3 + 2
    case ("B", "Z") => 6 + 3
    case ("C", "X") => 6 + 1
    case ("C", "Y") => 0 + 2
    case ("C", "Z") => 3 + 3
  }
  .sum

  // A = Rock, B = Paper, C = Scissors
  // X = Lose, Y = Draw, Z = Win
  def part2(input: Seq[String]): Int = parse(input).map {
    case ("A", "X") => 0 + 3
    case ("A", "Y") => 3 + 1
    case ("A", "Z") => 6 + 2
    case ("B", "X") => 0 + 1
    case ("B", "Y") => 3 + 2
    case ("B", "Z") => 6 + 3
    case ("C", "X") => 0 + 2
    case ("C", "Y") => 3 + 3
    case ("C", "Z") => 6 + 1
  }
  .sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day02.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
