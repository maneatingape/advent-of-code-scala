package AdventOfCode2015

import collection.mutable.ArrayBuffer

object Day10:
  def step(initial: ArrayBuffer[Char]): ArrayBuffer[Char] =
    val buffer = ArrayBuffer[Char]()
    initial.appended('x').foldLeft(0 -> initial.head) { case ((count, previous), next) =>
      if previous == next then
        (count + 1, previous)
      else
        buffer.append((count + '0').toChar).append(previous)
        (1, next)
    }
    buffer

  def generate(input: String, count: Int): Int = Iterator.iterate(ArrayBuffer.from(input))(step).drop(count).next().size

  def part1(input: String): Int = generate(input, 40)

  def part2(input: String): Int = generate(input, 50)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day10.txt").mkString.trim
    println(part1(data))
    println(part2(data))
