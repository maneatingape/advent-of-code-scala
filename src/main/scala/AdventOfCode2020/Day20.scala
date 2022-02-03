package AdventOfCode2020

object Day20:
  //                   #
  // #    ##    ##    ###
  //  #  #  #  #  #  #
  val seaMonster = Seq((0, 1), (1, 2), (4, 2), (5, 1), (6, 1), (7, 2), (10, 2), (11, 1), (12, 1), (13, 2), (16, 2), (17, 1), (18, 0), (18, 1), (19, 1))

  case class Point(x: Int, y: Int):
    def updated(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  case class Image(size: Int, pixels: Map[Point, Char]):
    val top = (0 until size).map(x => pixels(Point(x, 0)))
    val left = (0 until size).map(y => pixels(Point(0, y)))
    val bottom = (0 until size).map(x => pixels(Point(x, size - 1)))
    val right = (0 until size).map(y => pixels(Point(size - 1, y)))

    private def rotateClockwise = Image(size, pixels.map((point, pixel) => Point(size - point.y - 1, point.x) -> pixel))
    private def flipHorizontal = Image(size, pixels.map((point, pixel) => Point(size - point.x - 1, point.y) -> pixel))
    private def flipVertical = Image(size, pixels.map((point, pixel) => Point(point.x, size - point.y - 1) -> pixel))

    def permutations: Seq[Image] =
      val all =
        for
          rotateClockwiseCount <- 0 to 3
          flipHorizontalCount <- 0 to 1
          flipVerticalCount <- 0 to 1
        yield
          val rotated = Iterator.iterate(this)(_.rotateClockwise).drop(rotateClockwiseCount).next()
          val flippedHorizontal = Iterator.iterate(rotated)(_.flipHorizontal).drop(flipHorizontalCount).next()
          val flippedVertical = Iterator.iterate(flippedHorizontal)(_.flipVertical).drop(flipVerticalCount).next()
          flippedVertical
      all.distinct
    end permutations
  end Image

  case class Tile(id: Long, image: Image):
    export image.{top, left, bottom, right}

    val edges = {
      val forward = Set(top, left, bottom, right)
      val backwards = forward.map(_.reverse)
      forward ++ backwards
    }

    def isNeighbour(other: Tile): Boolean = other.edges.intersect(edges).size > 0
    def permutations: Seq[Tile] = image.permutations.map(Tile(id, _))
  end Tile

  def parse(input: String): Seq[Tile] = input
    .trim.split("\n\n").toSeq
    .map(_.split("\n").map(_.trim))
    .map { tile =>
      val id = tile.head.slice(5, 9).toLong
      val data = tile.drop(1)
      val pixels = for y <- 0 to 9; x <- 0 to 9 yield Point(x, y) -> data(y)(x)
      Tile(id, Image(10, pixels.toMap))
    }

  def corners(tiles: Seq[Tile]): Seq[Tile] =
    // All tile edges match with only one other tile.
    // Therefore corner tiles will have 2 values with no matches and edge tiles will have 1 value with no matches.
    // Those values will occur only once whereas values that match with another tile will occur twice.
    val occurrences = tiles.map(_.edges).flatten.groupMapReduce(identity)(_ => 1)(_ + _)
    // Corners are grouped under key 4 (two edges unique), edges under key 2 and other tiles under key 0
    val categories = tiles.groupBy(tile => tile.edges.count(edge => occurrences(edge) == 1))
    categories(4)
  end corners

  def unscrambleTiles(tiles: Seq[Tile]): Map[Point, Tile] =
    val size = math.sqrt(tiles.size).toInt
    val points = for x <- 0 until size; y <- 0 until size yield Point(x,y)

    // Arbitrarily choose the first corner
    val corner = corners(tiles).head.permutations
    val occurrences = tiles.map(_.edges).flatten.groupMapReduce(identity)(_ => 1)(_ + _)

    def helper(remaining: Seq[Tile], points: Seq[Point], ordered: Map[Point, Tile]): Map[Point, Tile] =
      if points.isEmpty then ordered
      else
        val tile = points.head match
          case Point(0, 0) => corner.filter(tile => occurrences(tile.top) == 1 && occurrences(tile.left) == 1).head
          case Point(0, y) => remaining.filter(tile => tile.top == ordered(Point(0, y - 1)).bottom).head
          case Point(x, y) => remaining.filter(tile => tile.left == ordered(Point(x - 1, y)).right).head
        helper(remaining.filterNot(_.id == tile.id), points.tail, ordered.updated(points.head, tile))
    end helper

    helper(tiles.flatMap(_.permutations), points, Map())
  end unscrambleTiles

  def assembleImage(unscrambled: Map[Point, Tile]): Image =
    val size = 8 * math.sqrt(unscrambled.size).toInt
    val pixels = for
      x <- 0 until size
      y <- 0 until size
    yield Point(x, y) -> unscrambled(Point(x / 8, y / 8)).image.pixels(Point(1 + x % 8, 1 + y % 8))

    Image(size, pixels.toMap)
  end assembleImage

  def findSeaMonsters(image: Image): Seq[Int] =
    for
      candidate <- image.permutations
    yield
      val matches = for
        x <- 0 until image.size - 20 // Sea monster dimensions are 20 x 3
        y <- 0 until image.size - 3
        if seaMonster.map(Point(x, y).updated).forall(point => candidate.pixels(point) == '#')
      yield (x, y)
      val monsters = matches.map(Point(_, _)).flatMap(point => seaMonster.map(point.updated)).toSet
      candidate.pixels.keys.count(point => candidate.pixels(point) == '#' && !monsters.contains(point))
  end findSeaMonsters

  def part1(input: String): Long = corners(parse(input)).map(_.id).product

  def part2(input: String): Long =
    val tiles = parse(input)
    val unscrambled = unscrambleTiles(tiles)
    val completeImage = assembleImage(unscrambled)
    val roughness = findSeaMonsters(completeImage)
    roughness.min
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day20.txt").mkString
    println(part1(data))
    println(part2(data))
