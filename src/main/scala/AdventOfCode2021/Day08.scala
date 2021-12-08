package AdventOfCode2021

object Day08:
  val sample = Seq(
    //"acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

  def isKnownDigit(pattern: String) = pattern.length match
    case 2 | 3 | 4 | 7 => true
    case _ => false

  def part1(input: Seq[String]): Int = input
    .flatMap(_.split(" \\| ")(1).split(" ").toSeq)
    .count(isKnownDigit)

  def part2(input: Seq[String]): Int = {
    val foo = input.map(_.split(" \\| ").toSeq.map(_.split(" ").map(_.sorted).toSeq))
    foo.map(line => helper(line)).sum
  }

  def helper(foo: Seq[Seq[String]]): Int = {
    val full = foo(0)

    val routesCF = full.find(_.length == 2).get.toSet // One
    val routesACF = full.find(_.length == 3).get.toSet // Seven
    val routesBCDF = full.find(_.length == 4).get.toSet // Four
    val routeA = routesACF.diff(routesCF)

    val short = full.filterNot(isKnownDigit)
    val segments = List('a', 'b', 'c', 'd', 'e', 'f', 'g')

    val routesAG = segments.filter(c => short.flatMap(identity).count(_ == c) == 6).toSet
    val routeG = routesAG.diff(routeA)
    val routesBC = segments.filter(c => short.flatMap(identity).count(_ == c) == 4).toSet
    val routeC = routesACF.intersect(routesBC)
    val routeB = routesBC.diff(routeC)
    val routesBD = routesBCDF.diff(routesACF)
    val routeD = routesBD.diff(routeB)
    val routeE = segments.filter(c => short.flatMap(identity).count(_ == c) == 3).toSet
    val routeF = routesCF.diff(routeC)


    val first = Map(
      routeA.head -> 'a',
      routeB.head -> 'b',
      routeC.head -> 'c',
      routeD.head -> 'd',
      routeE.head -> 'e',
      routeF.head -> 'f',
      routeG.head -> 'g')

    val second = Map(
      "abcefg" -> 0,
      "cf" -> 1,
      "acdeg" -> 2,
      "acdfg" -> 3,
      "bcdf" -> 4,
      "abdfg" -> 5,
      "abdefg" -> 6,
      "acf" -> 7,
      "abcdefg" -> 8,
      "abcdfg" -> 9)


    val bar = foo(1)
    val wix = bar.map(x => second(x.map(first).sorted))
    val (number, _) = wix.foldRight((0, 1)) { case (digit, (total, power)) =>
      (total + digit * power, power * 10)
    }

    println(bar)
    println(wix)
    println(number)
    number
  }

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day08.txt").getLines().toSeq
    //println(part1(data))
    println(part2(data))