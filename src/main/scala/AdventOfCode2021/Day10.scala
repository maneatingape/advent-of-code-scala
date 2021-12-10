package AdventOfCode2021

object Day10:


  def part1(input: Seq[String]): Int =
    val results = input.map { line =>
      val (stack, state) = line.foldLeft((List('x'), 'x')) { case ((stack, state), next) =>
        if state != 'x' then
          (stack, state)
        else next match
          case '(' | '[' | '{' | '<' => (next :: stack, state)
          case ')' => if stack.head == '(' then (stack.tail, state) else (stack, next)
          case ']' => if stack.head == '[' then (stack.tail, state) else (stack, next)
          case '}' => if stack.head == '{' then (stack.tail, state) else (stack, next)
          case '>' => if stack.head == '<' then (stack.tail, state) else (stack, next)
      }

      state match
        case 'x' => 0
        case ')' => 3
        case ']' => 57
        case '}' => 1197
        case '>' => 25137
    }
    results.sum


  def part2(input: Seq[String]): Long =
    val results = input.map { line =>
      val (stack, state) = line.foldLeft((List('x'), 'x')) { case ((stack, state), next) =>
        if state != 'x' then
          (stack, state)
        else next match
          case '(' | '[' | '{' | '<' => (next :: stack, state)
          case ')' => if stack.head == '(' then (stack.tail, state) else (stack, next)
          case ']' => if stack.head == '[' then (stack.tail, state) else (stack, next)
          case '}' => if stack.head == '{' then (stack.tail, state) else (stack, next)
          case '>' => if stack.head == '<' then (stack.tail, state) else (stack, next)
      }

      if state == 'x' then
        stack.dropRight(1).foldLeft(0L) { case (total, next) =>
          val score = next match
            case '(' => 1L
            case '[' => 2L
            case '{' => 3L
            case '<' => 4L

          5L * total + score
        }
      else -1
    }
    val sorted = results.filterNot(_ == -1).sorted
    sorted(sorted.length / 2)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day10.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
