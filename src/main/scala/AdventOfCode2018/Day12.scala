package AdventOfCode2018

object Day12:
  def parse(input: Seq[String]): (Set[Int], Set[String]) =
    val plants = input.head.drop(15).zipWithIndex.filter(_._1 == '#').map(_._2).toSet
    val rules = input.drop(2).filter(_.last == '#').map(_.take(5)).toSet
    (plants, rules)

  def step(plants: Set[Int], rules: Set[String]): Set[Int] =
    val candidates = for plantIndex <- plants.min - 2 to plants.max + 2 yield
      val pattern = (-2 to 2).foldLeft("") { (total, offset) =>
        total + (if plants.contains(plantIndex + offset) then '#' else '.')
      }
      Option.when(rules.contains(pattern))(plantIndex)
    candidates.flatten.toSet

  def part1(input: Seq[String]): Int =
    val (plants, rules) = parse(input)
    Iterator.iterate(plants)(step(_, rules)).drop(20).next().sum

  def part2(input: Seq[String]): Long =
    val (plants, rules) = parse(input)

    def helper(plants: Set[Int], previousDelta: Int, generation: Int): Long =
      val next = step(plants, rules)
      val delta = next.sum - plants.sum
      if delta == previousDelta then next.sum + delta * (50000000000L - generation)
      else helper(next, delta, generation + 1)

    helper(plants, 0, 1)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day12.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
