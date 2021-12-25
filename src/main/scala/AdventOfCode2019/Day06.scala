package AdventOfCode2019

object Day06:
  def parse(input: Seq[String]): Map[String, List[String]] =
    val parent = input.map(_.split("\\)")).map(a => a(1) -> a(0)).toMap
    val cache = collection.mutable.Map("COM" -> List("COM"))
    def orbit(planet: String): List[String] = cache.getOrElseUpdate(planet, planet :: orbit(parent(planet)))
    parent.map((planet, _) => planet -> orbit(planet))

  def part1(input: Seq[String]): Int = parse(input).values.map(_.tail.size).sum

  def part2(input: Seq[String]): Int =
    val orbits = parse(input)
    val (you, san) = (orbits("YOU"), orbits("SAN"))
    you.tail.indexWhere(san.contains) + san.tail.indexWhere(you.contains)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day06.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
