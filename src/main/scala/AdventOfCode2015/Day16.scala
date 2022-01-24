package AdventOfCode2015

object Day16:
  val constraints = Map(
    "children" -> 3, "cats" -> 7, "samoyeds" -> 2, "pomeranians" -> 3, "cars" -> 2,
    "vizslas" -> 0, "goldfish" -> 5, "trees" -> 3, "akitas" -> 0, "perfumes" -> 1)

  case class Aunt(id: Int, details: Map[String, Int])

  def parse(input: Seq[String]): Seq[Aunt] = input.map { line =>
    val tokens = line.drop(4).split("[: ,]+")
    Aunt(tokens.head.toInt, tokens.tail.grouped(2).map(w => w(0) -> w(1).toInt).toMap)
  }

  def solve(input: Seq[String])(predicate: ((String, Int)) => Boolean): Int =
    parse(input).find(_.details.forall(predicate)).get.id

  def part1(input: Seq[String]): Int = solve(input)((key, value) => value == constraints(key))

  def part2(input: Seq[String]): Int = solve(input) { (key, value) =>
    key match
      case "cats" | "trees" => value > constraints(key)
      case "pomeranians" | "goldfish" => value < constraints(key)
      case _ => value == constraints(key)
  }

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day16.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
