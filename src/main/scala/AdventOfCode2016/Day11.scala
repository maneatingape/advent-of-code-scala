package AdventOfCode2016

object Day11:
  sealed trait RTG
  case class Chip(name: String) extends RTG
  case class Generator(name: String) extends RTG

  case class State(elevator: Int, floors: Seq[Set[RTG]]):
    def finished: Boolean = elevator == 3 && floors.take(3).forall(_.isEmpty)
    def valid: Boolean = floors.forall { floor =>
      val (chips, generators) = floor.partitionMap {
        case Chip(name) => Left(name)
        case Generator(name) => Right(name)
      }
      generators.isEmpty || chips.subsetOf(generators)
    }
    def candidates: Seq[State] =
      val all = for
        subset <- floors(elevator).subsets(1) ++ floors(elevator).subsets(2)
        destination <- Set(elevator + 1, elevator - 1).filter(floors.indices.contains)
      yield State(destination, floors.updated(elevator, floors(elevator) -- subset).updated(destination, floors(destination) ++ subset))
      all.filter(_.valid).toSeq
    def equivalent: Seq[(Int, Int)] =
      floors.map { floor =>
        floor.foldLeft((0, 0)) { case ((chips, generators), next) =>
          next match
            case Chip(_) => (chips + 1, generators)
            case Generator(_) => (chips, generators + 1)
        }
      }
      .appended((elevator, elevator))

  def parse(input: Seq[String]): State =
    val chipRegex = "(\\w+)(?=-compatible)".r
    val generatorRegex = "(\\w+)(?= generator)".r
    val floors = input.map(line => (chipRegex.findAllIn(line).map(Chip) ++ generatorRegex.findAllIn(line).map(Generator)).toSet)
    State(0, floors)

  def dijkstra(start: State): Int =
    val cache = collection.mutable.Map(start.equivalent -> 0)
    val todo = collection.mutable.PriorityQueue(start -> 0)(Ordering.by((_,c) => -c))

    while todo.nonEmpty do
      val (current, total) = todo.dequeue()
      if current.finished then return total
      current.candidates.foreach { next =>
        val cost = total + 1
        val hash = next.equivalent
        if !cache.contains(hash) || cost < cache(hash) then
          cache(hash) = cost
          todo.enqueue(next -> cost)
      }

    -1
  end dijkstra

  def part1(input: Seq[String]): Int = dijkstra(parse(input))

  def part2(input: Seq[String]): Int =
    val state = parse(input)
    val extra = Set(Chip("elerium"), Generator("elerium"), Chip("dilithium"), Generator("dilithium"))
    val expanded = state.copy(floors = state.floors.updated(0, state.floors(0) ++ extra))
    dijkstra(expanded)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day11.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
