package AdventOfCode2022

object Day18:
  val orthogonal = Seq((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1))

  case class Cube(x: Int, y: Int, z: Int):
    def delta(dx: Int, dy: Int, dz: Int): Cube = Cube(x + dx, y + dy, z + dz)
    def neighbours: Seq[Cube] = orthogonal.map(delta)

  def parse(input: Seq[String]): Set[Cube] = input.toSet.map { line =>
    val Array(x, y, z) = line.split(",").map(_.toInt)
    Cube(x, y, z)
  }

  def part1(input: Seq[String]): Int =
    val cubes = parse(input)
    cubes.toSeq.map(_.neighbours.filterNot(cubes.contains).size).sum

  def part2(input: Seq[String]): Int =
    val cubes = parse(input)

    val xs = cubes.map(_.x).min - 1 to cubes.map(_.x).max + 1
    val ys = cubes.map(_.y).min - 1 to cubes.map(_.y).max + 1
    val zs = cubes.map(_.z).min - 1 to cubes.map(_.z).max + 1

    val start = Cube(xs.head, ys.head, zs.head)
    val todo = collection.mutable.Queue(start)
    val visited = collection.mutable.Set(start)

    while todo.nonEmpty do
      val cube = todo.dequeue()
      visited += cube
      cube.neighbours.filterNot(cubes.contains).filterNot(visited.contains).foreach { next =>
        if xs.contains(next.x) && ys.contains(next.y) && zs.contains(next.z) then
          todo.enqueue(next)
          visited += next
      }

    cubes.toSeq.map(_.neighbours.count(visited.contains)).sum
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day18.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
