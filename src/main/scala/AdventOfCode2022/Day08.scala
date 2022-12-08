package AdventOfCode2022

object Day08:
  case class Point(x: Int, y: Int)

  def part1(input: Seq[String]): Int =
    val grid = input.map(_.map(_.asDigit))
    val n = grid.size - 1
    val visible = collection.mutable.Set[Point]()

    def compare(x: Int, y: Int, tallest: Int): Int =
      val tree = grid(y)(x)
      if tree > tallest then visible += Point(x, y)
      tree.max(tallest)

    for i <- 0 to n do
      var (left, right, top, bottom) = (-1, -1, -1, -1)
      for j <- 0 to n do
        left = compare(j, i, left)
        right = compare(n - j, i, right)
        top = compare(i, j, top)
        bottom = compare(i, n - j, bottom)

    visible.size
  end part1


  def part2(input: Seq[String]): Int =
    val grid = input.map(_.map(_.asDigit))
    val n = grid.size - 1
    var score = 0

    for y <- 0 to n do
      for x <- 0 to n do
        val tree = grid(y)(x)
        def left: Int =
          for left <- (x - 1) to 0 by -1 do
            if grid(y)(left) >= tree then return x - left
          x
        def right: Int =
          for right <- (x + 1) to n do
            if grid(y)(right) >= tree then return right - x
          n - x
        def top: Int =
          for top <- (y - 1) to 0 by -1 do
            if grid(top)(x) >= tree then return y - top
          y
        def bottom: Int =
          for bottom <- (y + 1) to n do
            if grid(bottom)(x) >= tree then return bottom - y
          n - y
        score = score.max(left * right * bottom * top)

    score
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day08.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
