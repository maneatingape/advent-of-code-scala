package AdventOfCode2019

import scala.annotation.tailrec
import Day09.IntCode

object Day23:
  case class State(computer: IntCode, input: Seq[Long], output: Seq[Long])

  def step(network: Seq[State], nat: Seq[Long]): (Seq[State], Seq[Long]) =
    val nextNextwork = network.map { case State(computer, input, output) =>
      val nextComputer =
        if computer.input.nonEmpty then computer.next
        else if input.nonEmpty then computer.withInput(input*).next
        else computer.withInput(-1).next

      val nextInput = if computer.input.nonEmpty then input else Seq()

      val nextOutput = nextComputer.result match
        case IntCode.Output(value) => output.appended(value)
        case _ => output

      State(nextComputer, nextInput, nextOutput)
    }
    nextNextwork.zipWithIndex.foldLeft((nextNextwork, nat)) { case ((network, nat), (state, src)) =>
      if state.output.length != 3 then (network, nat)
      else
        val dest = state.output.head.toInt
        val nextSrc = state.copy(output = Seq())
        if dest == 255 then
          (network.updated(src, nextSrc), state.output.tail)
        else
          val nextDest = network(dest).copy(input = network(dest).input ++ state.output.tail)
          (network.updated(src, nextSrc).updated(dest, nextDest), nat)
    }
  end step

  def part1(memory: Seq[Long]): Long =
    @tailrec
    def helper(network: Seq[State], nat: Seq[Long]): Long = step(network, nat) match
      case (_, Seq(_, y)) => y
      case (nextNetwork, nextNat) => helper(nextNetwork, nextNat)

    val network = Seq.tabulate(50)(i => State(IntCode(memory), Seq(i), Seq()))
    helper(network, Seq())
  end part1

  def part2(memory: Seq[Long]): Long =
    @tailrec
    def helper(network: Seq[State], nat: Seq[Long], idleCount: Int, previousIdleY: Option[Long]): Long =
      val (nextNetwork, nextNat, nextIdleCount, nextIdleY) =
        if idleCount < 700 then
          val (nextNetwork, nextNat) = step(network, nat)
          val active = network.exists(state => state.input.nonEmpty || state.output.nonEmpty)
          val nextIdleCount = if active then 0 else idleCount + 1
          (nextNetwork, nextNat, nextIdleCount, None)
        else
          val nextNetwork = network.updated(0, network(0).copy(input = nat))
          (nextNetwork, nat, 0, Some(nat.last))

      (previousIdleY, nextIdleY) match
        case (Some(previous), Some(next)) if previous == next => next
        case _ => helper(nextNetwork, nextNat, nextIdleCount, nextIdleY.orElse(previousIdleY))
    end helper

    val network = Seq.tabulate(50)(i => State(IntCode(memory), Seq(i), Seq()))
    helper(network, Seq(), 0, None)
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day23.txt").mkString.trim.split(",").map(_.toLong).toSeq
    println(part1(data))
    println(part2(data))
