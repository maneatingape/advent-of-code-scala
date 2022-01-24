package AdventOfCode2015

object Day19:
  type Rules = Set[(scala.util.matching.Regex, Seq[String])]

  def parse(input: Seq[String]): (Seq[(String, String)], String) =
    val index = input.indexOf("")
    val pairs = input.take(index).map(_.split(" => ")).map(a => a(0) -> a(1))
    (pairs, input(index + 1))

  def makeRules(pairs: Seq[(String, String)]): Rules =
    pairs.map((k,v) => (k.r, v)).groupMap(_._1)(_._2).toSet

  def candidates(rules: Rules, input: String): Set[String] = rules.flatMap { case (key, values) =>
    key.findAllMatchIn(input).flatMap(mat => values.map(input.patch(mat.start, _, mat.end - mat.start)))
  }

  def aStar(rules: Rules, start: String): Int =
    val cost = collection.mutable.Map(start -> 0)
    val todo = collection.mutable.PriorityQueue(start -> 0)(Ordering.by(-_._2))

    while todo.nonEmpty do
      val (current, _) = todo.dequeue()
      if current == "e" then return cost(current)
      val nextCost = cost(current) + 1

      candidates(rules, current).foreach { next =>
        if !cost.contains(next) || nextCost < cost(next) then
          cost(next) = nextCost
          val priority = nextCost + next.size
          todo.enqueue(next -> priority)
      }
    end while

    -1
  end aStar

  def part1(input: Seq[String]): Int =
    val (pairs, start) = parse(input)
    val rules = makeRules(pairs)
    candidates(rules, start).size

  def part2(input: Seq[String]): Int =
    val (pairs, start) = parse(input)
    val rules = makeRules(pairs.map(_.swap))
    aStar(rules, start)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day19.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
