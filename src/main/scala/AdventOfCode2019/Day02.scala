package AdventOfCode2019

object Day02:
  def exec(code: Seq[Int]): Int =
    def helper(ip: Int, code: Seq[Int]): Int =
      code(ip) match
        case 1 => helper(ip + 4, code.updated(code(ip + 3), code(code(ip + 1)) + code(code(ip + 2))))
        case 2 => helper(ip + 4, code.updated(code(ip + 3), code(code(ip + 1)) * code(code(ip + 2))))
        case 99 => code.head
    helper(0, code)
  end exec

  def part1(input: Seq[Int]): Int = exec(input.updated(1, 12).updated(2, 2))

  def part2(input: Seq[Int]): Int =
    val permutations = for noun <- 1 to 99; verb <- 1 to 99 yield (noun, verb)
    val (noun, verb) = permutations.dropWhile((noun, verb) => exec(input.updated(1, noun).updated(2, verb)) != 19690720).head
    100 * noun + verb

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day02.txt").mkString.trim.split(",").map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
