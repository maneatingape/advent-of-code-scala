package AdventOfCode2019

object Day14:
  case class Reaction(output: Int, inputs: Seq[(String, Int)])

  type Inventory = Map[String, Long]
  extension (inventory: Inventory)
    def modify(chemical: String, amount: Long): Inventory = inventory.updated(chemical, inventory(chemical) + amount)

  def parse(input: Seq[String]): Map[String, Reaction] =
    def pair(s: String): (String, Int) =
      val Array(amount, chemical) = s.split(" ")
      (chemical, amount.toInt)

    input.map { line =>
      val Array(first, second) = line.split(" => ")
      val (chemical, output) = pair(second)
      chemical -> Reaction(output, first.split(", ").map(pair).toSeq)
    }
    .toMap
  end parse

  def fuel(reactions: Map[String, Reaction], amount: Long): Long =
    def ceil(a: Long, b: Long) = if a % b == 0 then a / b else a / b + 1

    def make(chemical: String, amount: Long, inventory: Inventory): Inventory =
      if chemical == "ORE" || inventory(chemical) >= amount then
        inventory.modify(chemical, -amount)
      else
        val extra = amount - inventory(chemical)
        val multiplier = ceil(extra, reactions(chemical).output)
        reactions(chemical).inputs.foldLeft(inventory) { case (inventory, (chemical, amount)) =>
          make(chemical, amount * multiplier, inventory)
        }
        .modify(chemical, multiplier * reactions(chemical).output - amount)
    end make

    val result = make("FUEL", amount, Map().withDefaultValue(0L))
    -result("ORE")
  end fuel

  def part1(input: Seq[String]): Long = fuel(parse(input), 1)

  def part2(input: Seq[String]): Long =
    val reactions = parse(input)
    val threshold = 1000000000000L

    def binarySearch(start: Long, end: Long): Long = if start >= end then start else
      val middle = (start + end) / 2
      val cost = fuel(reactions, middle)
      if cost > threshold then binarySearch(start, middle - 1)
      else if cost < threshold then binarySearch(middle + 1, end)
      else middle

    binarySearch(1, threshold)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day14.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
