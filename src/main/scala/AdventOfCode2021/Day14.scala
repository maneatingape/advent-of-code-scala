package AdventOfCode2021

object Day14:
  type Quantity = Map[Char, Long]
  type Pairs = Map[String, Long]

  def insertion(input: String, steps: Int): Long =
    val Array(polymer, tail) = input.trim.split("\n\n")
    val rules = tail.split("\n").map(_.trim.split(" -> ")).map(a => a(0) -> a(1)).toMap

    val quantity = polymer.foldLeft[Quantity](Map().withDefaultValue(0L)) { (map, element) =>
      map.updated(element, map(element) + 1L)
    }

    val pairs = polymer.sliding(2).foldLeft[Pairs](Map().withDefaultValue(0L)) { case (map, pair) =>
      map.updated(pair, map(pair) + 1L)
    }

    def step(pairs: Pairs, quantity: Quantity): (Pairs, Quantity) =
      pairs.keys.foldLeft[(Pairs, Quantity)]((Map().withDefaultValue(0L), quantity)) { case ((map, quantity), key) =>
        val first = key.head + rules(key)
        val second = rules(key) + key.last

        val nextPairs = map.updated(first, map(first) + pairs(key)).updated(second, map(second) + pairs(key))
        val nextQuantity = quantity.updated(rules(key).head, quantity(rules(key).head) + pairs(key))

        (nextPairs, nextQuantity)
      }

    val (_, wix) = (0 until steps).foldLeft((pairs, quantity)) { case ((pairs, quantity), index) =>
      step(pairs, quantity)
    }

    wix.values.max - wix.values.min


  def part1(input: String): Long = insertion(input, 10)

  def part2(input: String): Long = insertion(input, 40)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day14.txt").mkString
    println(part1(data))
    println(part2(data))
