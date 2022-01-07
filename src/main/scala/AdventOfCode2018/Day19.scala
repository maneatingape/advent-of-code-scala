package AdventOfCode2018

// Reverse engineering the instructions shows that the code is computing
// the sum of every factor of an integer value.
// The original code implements this in 0(N^2) quadratic complexity using 2 nested loops.
// Switching the first register to 1 computes a larger (and hence dramatically slower) value
// The hardcoded values for part 1 and part2 are extracted from reading and hand stepping
// through the code.
object Day19:
  def factorSum(n: Int, x: Int = 1, total: Int = 0): Int =
    if n % x == 0 then
      val y = n / x
      if y < x then total else factorSum(n, x + 1, total + x + y)
    else factorSum(n, x + 1, total)

  def part1: Int = factorSum(950)

  def part2: Int = factorSum(10551350)

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)
