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

  def explore(input: Seq[String], youInitial: Int, elephantInitial: Int): Int =
    val (valves, distance, todo) = parse(input)
    val cache = collection.mutable.Map[(Set[String], Set[String]), Int]().withDefaultValue(-1)

    def step(todo: Set[String], you: String, elephant: String, youTime: Int, elephantTime: Int, pressure: Int): Unit =
      val key = (Set(you, elephant), todo)
      if cache(key) >= pressure then return else cache(key) = pressure

      for next <- todo do
        val remaining = youTime - distance(you)(next)
        if remaining > 0 then
          val extra = remaining * valves(next).flow
          step(todo - next, next, elephant, remaining, elephantTime, pressure + extra)

      for next <- todo do
        val remaining = elephantTime - distance(elephant)(next)
        if remaining > 0 then
          val extra = remaining * valves(next).flow
          step(todo - next, you, next, youTime, remaining, pressure + extra)
    end step

    step(todo, "AA", "AA", youInitial, elephantInitial, 0)
    cache.values.max
  end explore

  def part1(input: Seq[String]): Int = explore(input, 30, 0)

  def part2(input: Seq[String]): Int = explore(input, 26, 26)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day16.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
