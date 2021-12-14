package AdventOfCode2021

import AdventOfCode2021.Day14.Totals

object Day14:
  type Totals = Map[String, Long]

  extension (map: Totals)
    def increase(key: String, amount: Long): Totals = map.updated(key, map.getOrElse(key, 0L) + amount)

  def insertion(input: String, steps: Int): Long =
    val Array(polymer, tail) = input.trim.split("\n\n")
    val rules = tail.split("\n").map(_.trim.split(" -> ")).map(a => a(0) -> a(1)).toMap

    val quantity = polymer.foldLeft[Totals](Map())((map, element) => map.increase(element.toString, 1))
    val pairs = polymer.sliding(2).foldLeft[Totals](Map())((map, pair) => map.increase(pair, 1))

    def step(pairs: Totals, quantity: Totals): (Totals, Totals) =
      pairs.foldLeft[(Totals, Totals)]((Map(), quantity)) { case ((pairs, quantity), (key, value)) =>
        val first = key.head + rules(key)
        val second = rules(key) + key.last

        val nextPairs = pairs.increase(first, value).increase(second, value)
        val nextQuantity = quantity.increase(rules(key), value)

        (nextPairs, nextQuantity)
      }

    val (_, result) = Iterator.iterate((pairs, quantity))(step).drop(steps).next()
    result.values.max - result.values.min


  def part1(input: String): Long = insertion(input, 10)

  def part2(input: String): Long = insertion(input, 40)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day14.txt").mkString
    println(part1(data))
    println(part2(data))
