package AdventOfCode2017

object Day17:
  def part1(input: Int): Int =
    var index = 0
    val buffer = collection.mutable.ArrayBuffer(0)

    for i <- 1 to 2017 do
      index = (index + input + 1) % i
      buffer.insert(index, i)

    buffer(index + 1)
  end part1

  def part2(input: Int): Int =
    var index = 0
    var result = -1

    for i <- 1 to 50000000 do
      index = (index + input + 1) % i
      if index == 0 then result = i

    result
  end part2

  def main(args: Array[String]): Unit =
    val data = 304
    println(part1(data))
    println(part2(data))