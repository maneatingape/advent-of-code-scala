package AdventOfCode2020

object Day16:
  case class Rule(pattern: String):
    private val regex = ".+: (\\d+)-(\\d+) or (\\d+)-(\\d+)".r
    private val (a, b, c, d) = pattern match {
      case regex(a, b, c, d) => (a.toInt, b.toInt, c.toInt, d.toInt)
    }
    def check(i: Int): Boolean = (a <= i && i <= b) || (c <= i && i <= d)

  def parse(input: Seq[String]): (Set[Rule], Seq[Int], Seq[Seq[Int]]) =
    val (rules, rest) = input.splitAt(input.indexOf(""))
    val (your, nearby) = (rest(2), rest.drop(5))
    (rules.map(Rule.apply).toSet, your.split(",").map(_.toInt).toSeq, nearby.map(_.split(",").map(_.toInt).toSeq))

  def part1(input: Seq[String]): Long =
    val (rules, your, nearby) = parse(input)
    nearby.flatMap(ticket => ticket.filterNot(field => rules.exists(rule => rule.check(field)))).sum

  def part2(input: Seq[String]): Long =
    val (rules, your, nearby) = parse(input)
    val valid = nearby.filter(ticket => ticket.forall(field => rules.exists(rule => rule.check(field))))
    val sets = valid.transpose.map(fields => rules.filter(rule => fields.forall(field => rule.check(field))))

    def refine(sets: Seq[Set[Rule]]): Seq[Set[Rule]] =
      val definite = sets.filter(_.size == 1).flatten
      sets.map(set => if set.size == 1 then set else set -- definite)

    val found = Iterator.iterate(sets)(refine).dropWhile(sets => sets.exists(set => set.size > 1)).next().map(_.head)
    found.zip(your).filter((rule, field) => rule.pattern.startsWith("departure")).map(_._2.toLong).product
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day16.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
