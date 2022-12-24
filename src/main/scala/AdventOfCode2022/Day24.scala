package AdventOfCode2022

object Day24:
  case class Point(x: Int, y: Int, z: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y, z + other.z)

  def part1(input: Seq[String]): Int = 123

  def part2(input: Seq[String]): Int = 456

  def parse(input: Seq[String]): (Set[Point], Set[Point], Set[Point], Set[Point], Set[Point]) =
    val right = for y <- input.indices; x <- input.head.indices if input(y)(x) == '>' yield Point(x, y, 0)
    val down = for y <- input.indices; x <- input.head.indices if input(y)(x) == 'v' yield Point(x, y, 0)
    val left = for y <- input.indices; x <- input.head.indices if input(y)(x) == '<' yield Point(x, y, 0)
    val up = for y <- input.indices; x <- input.head.indices if input(y)(x) == '^' yield Point(x, y, 0)
    val wall = for y <- input.indices; x <- input.head.indices if input(y)(x) == '#' yield Point(x, y, 0)
    (right.toSet, down.toSet, left.toSet, up.toSet, wall.toSet)

  def main(args: Array[String]): Unit =
    //    val data = sample
    //    val minX = 1
    //    val maxX = 6
    //    val minY = 1
    //    val maxY = 4

    val data = io.Source.fromResource("AdventOfCode2022/Day24.txt").getLines().toSeq
    val minX = 1
    val maxX = 120
    val minY = 1
    val maxY = 25

    val points = collection.mutable.Set[Point]()
    var (right, down, left, up, wall) = parse(data)
    wall = wall + Point(1, -1, 0)
    points ++= right
    points ++= down
    points ++= left
    points ++= down
    points ++= wall

    for _ <- 1 to 1000 do // Guess max z value
      right = right.map { cur =>
        if cur.x == maxX then Point(minX, cur.y, cur.z + 1) else Point(cur.x + 1, cur.y, cur.z + 1)
      }
      left = left.map { cur =>
        if cur.x == minX then Point(maxX, cur.y, cur.z + 1) else Point(cur.x - 1, cur.y, cur.z + 1)
      }
      up = up.map { cur =>
        if cur.y == minY then Point(cur.x, maxY, cur.z + 1) else Point(cur.x, cur.y - 1, cur.z + 1)
      }
      down = down.map { cur =>
        if cur.y == maxY then Point(cur.x, minY, cur.z + 1) else Point(cur.x, cur.y + 1, cur.z + 1)
      }
      wall = wall.map { cur =>
        Point(cur.x, cur.y, cur.z + 1)
      }
      points ++= right
      points ++= down
      points ++= left
      points ++= up
      points ++= wall
    end for

    def bfs(start: Point, end: Point): Int =
      val todo = collection.mutable.Queue(start)
      val cost = collection.mutable.Map(start -> 0)

      while todo.nonEmpty do
        val cur = todo.dequeue()

        if cur.x == end.x && cur.y == end.y then return cost(cur)

        val neighbours = Seq(
          Point(cur.x, cur.y, cur.z + 1),
          Point(cur.x - 1, cur.y, cur.z + 1),
          Point(cur.x + 1, cur.y, cur.z + 1),
          Point(cur.x, cur.y - 1, cur.z + 1),
          Point(cur.x, cur.y + 1, cur.z + 1),
        )

        neighbours.filterNot(cost.contains).filterNot(points.contains).foreach { next =>
          todo += next
          cost(next) = cost(cur) + 1
        }
      end while

      -1
    end bfs

    val time1 = bfs(Point(1, 0, 0), Point(maxX, maxY + 1, 0))
    println(time1)
    val time2 = bfs(Point(maxX, maxY + 1, time1), Point(1, 0, 0))
    println(time2)
    val time3 = bfs(Point(1, 0, time1 + time2), Point(maxX, maxY + 1, 0))
    println(time3)
    println(time1 + time2 + time3)
  end main
