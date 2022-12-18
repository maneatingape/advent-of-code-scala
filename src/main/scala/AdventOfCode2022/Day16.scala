package AdventOfCode2022

object Day16:
  case class Valve(flow: Int, neighbours: Seq[String])

  def parse(input: Seq[String]): (Map[String, Valve], Map[String, Map[String, Int]], Set[String]) =
    val valves = input
      .map { line =>
        val Array(_, name, flow, edges: _*) = line.split("[^A-Z0-9]+"): @unchecked
        name -> Valve(flow.toInt, edges)
      }
      .toMap
    val distance = valves.map((k, v) => k -> bfs(valves, k))
    val todo = valves.filter((k, v) => v.flow > 0).keySet
    (valves, distance, todo)

  def bfs(graph: Map[String, Valve], start: String): Map[String, Int] =
    val todo = collection.mutable.Queue(start)
    val cost = collection.mutable.Map(start -> 1)

    while todo.nonEmpty do
      val current = todo.dequeue()
      graph(current).neighbours.filterNot(cost.contains).foreach { next =>
        todo.enqueue(next)
        cost(next) = cost(current) + 1
      }

    cost.toMap
  end bfs

  def explore(input: Seq[String], initial: Int): Map[Set[String], Int] =
    val (valves, distance, todo) = parse(input)
    val score = collection.mutable.Map[Set[String], Int]().withDefaultValue(0)

    def step(todo: Set[String], done: Set[String], from: String, time: Int, pressure: Int): Unit =
      score(done) = score(done).max(pressure)
      for
        next <- todo
        remaining = time - distance(from)(next)
        if remaining > 0
        extra = remaining * valves(next).flow
      do step(todo - next, done + next, next, remaining, pressure + extra)
    end step

    step(todo, Set(), "AA", initial, 0)
    score.toMap
  end explore

  def part1(input: Seq[String]): Int = explore(input, 30).values.max

  def part2(input: Seq[String]): Int =
    val sets = explore(input, 26)
    val disjoint = for
      (you, score1) <- sets
      (elephant, score2) <- sets
      if you.intersect(elephant).size == 0
    yield score1 + score2
    disjoint.max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day16.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
