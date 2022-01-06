package AdventOfCode2018

object Day16:
  def gt(x: Int, y: Int): Int = if x > y then 1 else 0
  def eq(x: Int, y: Int): Int = if x == y then 1 else 0

  def exec(opcode: Int, a: Int, b: Int, c: Int, register: Seq[Int]): Int = opcode match
    case 0 => register(a) + register(b)     // addr
    case 1 => register(a) + b               // addi
    case 2 => register(a) * register(b)     // mulr
    case 3 => register(a) * b               // muli
    case 4 => register(a) & register(b)     // banr
    case 5 => register(a) & b               // bani
    case 6 => register(a) | register(b)     // borr
    case 7 => register(a) | b               // bori
    case 8 => register(a)                   // setr
    case 9 => a                             // seti
    case 10 => gt(a, register(b))           // gtir
    case 11 => gt(register(a), b)           // gtri
    case 12 => gt(register(a), register(b)) // gtrr
    case 13 => eq(a, register(b))           // eqir
    case 14 => eq(register(a), b)           // eqri
    case 15 => eq(register(a), register(b)) // eqrr

  case class Sample(before: Seq[Int], instruction: Seq[Int], after: Seq[Int])

  def parse(raw: Seq[String]): (Seq[Sample], Seq[Seq[Int]]) =
    val input = raw.map(_.split("\\D+").filter(_.nonEmpty).map(_.toInt).toSeq)
    val index = input.indexOfSlice(Seq(Seq(), Seq(), Seq()))
    val samples = input.take(index + 1).grouped(4).toSeq.map {
      case Seq(before, instruction, after, _) => Sample(before, instruction, after)
    }
    val program = input.drop(index + 3)
    (samples, program)

  def sets(samples: Seq[Sample]): Seq[Set[Int]] = samples.map { case Sample(before, Seq(_, a, b, c), after) =>
    (0 to 15).filter(opcode => before.updated(c, exec(opcode, a, b, c, before)) == after).toSet
  }

  def redirect(samples: Seq[Sample]): Map[Int, Int] =
    def helper(remaining: Seq[Set[Int]], redirect: Map[Int, Int]): Map[Int, Int] =
      if remaining.forall(_.isEmpty) then redirect else
        val index = remaining.indexWhere(_.size == 1)
        val from = samples(index).instruction(0)
        val to = remaining(index).head
        helper(remaining.map(_ - to), redirect.updated(from, to))

    helper(sets(samples), Map())
  end redirect

  def part1(input: Seq[String]): Int =
    val (samples, _) = parse(input)
    sets(samples).count(_.size >= 3)

  def part2(input: Seq[String]): Int =
    val (samples, program) = parse(input)
    val mapping = redirect(samples)
    val result = program.foldLeft(Seq(0, 0, 0, 0)) { case (register, Seq(opcode, a, b, c)) =>
      register.updated(c, exec(mapping(opcode), a, b, c, register))
    }
    result(0)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day16.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
