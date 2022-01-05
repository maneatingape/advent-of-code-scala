package AdventOfCode2018

object Day14:
  case class State(recipes: Vector[Int], firstIndex: Int, secondIndex: Int):
    def next: State =
      val first = recipes(firstIndex)
      val second = recipes(secondIndex)
      val sum = first + second

      val nextRecipes = if sum >= 10 then recipes.appended(sum / 10).appended(sum % 10) else recipes.appended(sum)
      val nextFirstIndex = (firstIndex + 1 + first) % nextRecipes.length
      val nextSecondIndex = (secondIndex + 1 + second) % nextRecipes.length

      State(nextRecipes, nextFirstIndex, nextSecondIndex)
    end next
  end State

  val initial = State(Vector(3, 7), 0, 1)

  def part1(target: Int): String = Iterator.iterate(initial)(_.next)
    .dropWhile(_.recipes.length < target + 10)
    .next().recipes.drop(target).take(10).mkString

  def part2(target: Int): Int =
    val digits = target.toString.map(_.asDigit)
    Iterator.iterate((initial, -1)) { (state, _) =>
      (state.next, state.recipes.indexOfSlice(digits, state.recipes.length - digits.length - 1))
    }
    .dropWhile(_._2 == -1)
    .next()._2

  def main(args: Array[String]): Unit =
    val input = 236021
    println(part1(input))
    println(part2(input))
