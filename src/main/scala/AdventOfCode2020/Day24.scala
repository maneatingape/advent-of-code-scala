package AdventOfCode2020

object Day24:
  case class Tile(x: Int, y: Int):
    def even: Boolean = y % 2 == 0
    def east: Tile = Tile(x + 1, y)
    def west: Tile = Tile(x - 1, y)
    def northeast: Tile = if even then Tile(x + 1, y + 1) else Tile(x, y + 1)
    def northwest: Tile = if even then Tile(x, y + 1) else Tile(x - 1, y + 1)
    def southeast: Tile = if even then Tile(x + 1, y - 1) else Tile(x, y - 1)
    def southwest: Tile = if even then Tile(x, y - 1) else Tile(x - 1, y - 1)
    def neighbours: Seq[Tile] = Seq(this, east, west, northeast, northwest, southeast, southwest)

  type Floor = Set[Tile]
  extension (floor: Floor) def adjacent(tile: Tile): Int = tile.neighbours.count(floor.contains)

  def parse(input: Seq[String]): Floor = input
    .map { directions =>
      def helper(point: Tile, remaining: String): Tile =
        if remaining.isEmpty then point else remaining.head match
          case 'e' => helper(point.east, remaining.tail)
          case 'w' => helper(point.west, remaining.tail)
          case 'n' => remaining(1) match
            case 'e' => helper(point.northeast, remaining.drop(2))
            case 'w' => helper(point.northwest, remaining.drop(2))
          case 's' => remaining(1) match
            case 'e' => helper(point.southeast, remaining.drop(2))
            case 'w' => helper(point.southwest, remaining.drop(2))
        end if
      helper(Tile(0, 0), directions)
    }
    .groupMapReduce(identity)(_ => 1)(_ + _)
    .filter(_._2 % 2 == 1)
    .keySet

  def part1(input: Seq[String]): Int = parse(input).size

  def part2(input: Seq[String]): Int =
    def step(floor: Set[Tile]): Set[Tile] = floor.flatMap(_.neighbours).flatMap { tile =>
      (floor.contains(tile), floor.adjacent(tile)) match
        case (_, 2) | (true, 3) => Some(tile)
        case _ => None
    }
    Iterator.iterate(parse(input))(step).drop(100).next().size

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day24.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
