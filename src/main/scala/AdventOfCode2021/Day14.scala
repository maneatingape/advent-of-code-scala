package AdventOfCode2021

object Day14:
  type Quantity = Map[Char, Long]
  type Pairs = Map[String, Long]

  extension [A](map: Map[A, Long])
    def increase(key: A, amount: Long): Map[A, Long] = map.updated(key, map.getOrElse(key, 0L) + amount)

  def insertion(input: Seq[String], steps: Int): Long =
    val quantity = input.head.groupMapReduce(identity)(_ => 1L)(_ + _)
    val pairs = input.head.sliding(2).toSeq.groupMapReduce(identity)(_ => 1L)(_ + _)
    val rules = input.drop(2).map(_.split(" -> ")).map(a => a(0) -> a(1).head).toMap

    def step(pairs: Pairs, quantity: Quantity): (Pairs, Quantity) =
      pairs.foldLeft[(Pairs, Quantity)]((Map(), quantity)) { case ((pairs, quantity), (key, value)) =>
        val nextPairs = pairs.increase(key.updated(0, rules(key)), value).increase(key.updated(1, rules(key)), value)
        val nextQuantity = quantity.increase(rules(key), value)
        (nextPairs, nextQuantity)
      }

    val (_, result) = Iterator.iterate((pairs, quantity))(step).drop(steps).next()
    result.values.max - result.values.min
  end insertion

  def part1(input: Seq[String]): Long = insertion(input, 10)

  def part2(input: Seq[String]): Long = insertion(input, 40)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day14.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
