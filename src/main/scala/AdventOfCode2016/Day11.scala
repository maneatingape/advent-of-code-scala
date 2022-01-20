package AdventOfCode2016

object Day11:
  val permutations = Seq(Floor(2, 0), Floor(1, 0), Floor(1, 1), Floor(0, 1), Floor(0, 2))
  val adjacent = Map(0 -> Seq(1), 1 -> Seq(0, 2), 2 -> Seq(1, 3), 3 -> Seq(2))

  case class Floor(microchips: Int, generators: Int):
    def empty: Boolean = microchips == 0 && generators == 0
    def valid: Boolean = (microchips >= 0 && generators >= 0) && (generators == 0 || microchips <= generators)
    def +(other: Floor): Floor = Floor(microchips + other.microchips, generators + other.generators)
    def -(other: Floor): Floor = Floor(microchips - other.microchips, generators - other.generators)

  case class State(elevator: Int, floors: Seq[Floor]):
    def finished: Boolean = elevator == 3 && floors.take(3).forall(_.empty)
    def valid: Boolean = floors.forall(_.valid)
    def candidates: Seq[State] =
      for
        adjust <- permutations
        destination <- adjacent(elevator)
      yield State(destination, floors.updated(elevator, floors(elevator) - adjust).updated(destination, floors(destination) + adjust))

  def parse(input: Seq[String]): State =
    val microchip = "microchip".r
    val generator = "generator".r
    State(0, input.map(line => Floor(microchip.findAllIn(line).size, generator.findAllIn(line).size)))

  def bfs(start: State): Int =
    val todo = collection.mutable.Queue(start)
    val cache = collection.mutable.Map(start -> 0)

    while todo.nonEmpty do
      val current = todo.dequeue()
      val cost = cache(current) + 1
      current.candidates.filter(_.valid).foreach { next =>
        if !cache.contains(next) || cost < cache(next) then
          cache(next) = cost
          todo.enqueue(next)
      }

    cache(cache.keys.filter(_.finished).head)
  end bfs

  def part1(input: Seq[String]): Int = bfs(parse(input))

  def part2(input: Seq[String]): Int =
    val State(elevator, floors) = parse(input)
    val expanded = State(elevator, floors.updated(0, floors(0) + Floor(2, 2)))
    bfs(expanded)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day11.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
