package AdventOfCode2020

object Day21:
  val regex = "(.+) \\(contains (.+)\\)".r

  def parse(input: Seq[String]): Seq[(Set[String], Set[String])] = input.map {
    case regex(ingredients, allergens) => (ingredients.split(" ").toSet, allergens.split(", ").toSet)
  }

  def partition(list: Seq[(Set[String], Set[String])]): (Map[String, Set[String]], Map[String, Set[String]]) =
    val candidates = list.foldLeft(Map.empty[String, Set[String]]) { case (candidates, (ingredients, allergens)) =>
      ingredients.foldLeft(candidates) { (candidates, ingredient) =>
        candidates.updated(ingredient, candidates.getOrElse(ingredient, Set()) ++ allergens)
      }
    }
    val reduced = candidates.map { (candidate, possible) =>
      val nextReduced = list.foldLeft(possible) { case (possible, (ingredients, allergens)) =>
        if ingredients.contains(candidate) then possible else possible -- allergens
      }
      (candidate, nextReduced)
    }

    reduced.partition((_, possible) => possible.isEmpty)
  end partition

  def findKnownIngredients(risky: Map[String, Set[String]]): String =
    def helper(remaining: Map[String, Set[String]], known: List[(String, String)]): List[(String, String)] =
      if remaining.isEmpty then known else
        val (ingredient, allergen) = remaining.find((_, allergens) => allergens.size == 1).get
        val nextRemaining = remaining.removed(ingredient).view.mapValues(_ - allergen.head).toMap
        val nextKnown = (ingredient, allergen.head) :: known
        helper(nextRemaining, nextKnown)
      end if
    end helper

    helper(risky, Nil).sortBy(_._2).map(_._1).mkString(",")
  end findKnownIngredients

  def part1(input: Seq[String]): Int =
    val list = parse(input)
    val (inert, risky) = partition(list)
    list.map((ingredients, _) => inert.keySet.intersect(ingredients).size).sum

  def part2(input: Seq[String]): String =
    val list = parse(input)
    val (inert, risky) = partition(list)
    findKnownIngredients(risky)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day21.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
