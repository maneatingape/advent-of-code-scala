package AdventOfCode2022

object Day16:
  case class Valve(flow: Int, neighbours: Seq[String])

  def parse(input: Seq[String]): (Map[String, Valve], Map[String, Map[String, Int]], Set[String]) =
    val valves = input
      .map { line =>
        val Array(_, name, flow, edges: _*) = line.split("[^A-Z0-9]+")
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

  def part1(input: Seq[String]): Int =
    val (valves, distance, todo) = parse(input)

    def explore(you: String, todo: Set[String], time: Int, pressure: Int): Int =
      val result = for
        next <- todo
        remaining = time - distance(you)(next)
        if remaining > 0
        extra = remaining * valves(next).flow
      yield explore(next, todo - next, remaining, pressure + extra)
      result.foldLeft(pressure)(_ max _)

    explore("AA", todo, 30, 0)
  end part1

  def part2(input: Seq[String]): Int =
    val (valves, distance, todo) = parse(input)
    val cache = collection.mutable.Map[(Set[String], Set[String]), Int]().withDefaultValue(-1)

    def explore(you: String, elephant: String, todo: Set[String], youTime: Int, elephantTime: Int, pressure: Int): Int =
      val key = (Set(you, elephant), todo)
      if cache(key) >= pressure then return -1 else cache(key) = pressure

      val first = for
        next <- todo
        remaining = youTime - distance(you)(next)
        if remaining > 0
        extra = remaining * valves(next).flow
      yield explore(next, elephant, todo - next, remaining, elephantTime, pressure + extra)

      val second = for
        next <- todo
        remaining = elephantTime - distance(elephant)(next)
        if remaining > 0
        extra = remaining * valves(next).flow
      yield explore(you, next, todo - next, youTime, remaining, pressure + extra)

      (first ++ second).foldLeft(pressure)(_ max _)
    end explore

    explore("AA", "AA", todo, 26, 26, 0)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day16.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
