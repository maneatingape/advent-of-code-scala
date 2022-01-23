# Advent of Code

Complete 2021 to 2016 entries for the annual [Advent of Code](https://adventofcode.com/) challenge, written in concise idiomatic functional Scala.

The coding style philosophy is [Readability](https://www.lihaoyi.com/post/StrategicScalaStylePrincipleofLeastPower.html) > [Simplicity](https://en.wikipedia.org/wiki/KISS_principle) > [Performance](https://www.laws-of-software.com/laws/knuth/).

As far as possible, each day's solution embraces the following approaches:
* Self-contained within a single file, depending only on the standard Scala library.
* [Immutable data structures](https://docs.scala-lang.org/scala3/book/fp-immutable-values.html), using copy-on-write to update.
* [Pure functions](https://en.wikipedia.org/wiki/Pure_function) with side effects, such as printing to the screen, restricted to a single `main` method.

The minimal SBT project provides:
* Straightforward consistent layout of code, resources and tests.
* Unit test coverage based on the sample input.
* Continuous integration using [GitHub Actions](https://docs.github.com/en/actions).

[![Scala CI](https://github.com/maneatingape/advent-of-code/actions/workflows/scala.yml/badge.svg)](https://github.com/maneatingape/advent-of-code/actions)

## Years

* [2021](#2021)
* [2020](#2020)
* [2019](#2019)
* [2018](#2018)
* [2017](#2017)
* [2016](#2016)
* [2015](#2015)

## 2021

| Day | Problem | Solution |
| --- | --- | --- |
| 1 | [Sonar Sweep](https://adventofcode.com/2021/day/1) | [Source](src/main/scala/AdventOfCode2021/Day01.scala) |
| 2 | [Dive!](https://adventofcode.com/2021/day/2) | [Source](src/main/scala/AdventOfCode2021/Day02.scala) |
| 3 | [Binary Diagnostic](https://adventofcode.com/2021/day/3) | [Source](src/main/scala/AdventOfCode2021/Day03.scala) |
| 4 | [Giant Squid](https://adventofcode.com/2021/day/4) | [Source](src/main/scala/AdventOfCode2021/Day04.scala) |
| 5 | [Hydrothermal Venture](https://adventofcode.com/2021/day/5) | [Source](src/main/scala/AdventOfCode2021/Day05.scala) |
| 6 | [Lanternfish](https://adventofcode.com/2021/day/6) | [Source](src/main/scala/AdventOfCode2021/Day06.scala) |
| 7 | [The Treachery of Whales](https://adventofcode.com/2021/day/7) | [Source](src/main/scala/AdventOfCode2021/Day07.scala) |
| 8 | [Seven Segment Search](https://adventofcode.com/2021/day/8) | [Source](src/main/scala/AdventOfCode2021/Day08.scala) |
| 9 | [Smoke Basin](https://adventofcode.com/2021/day/9) | [Source](src/main/scala/AdventOfCode2021/Day09.scala) |
| 10 | [Syntax Scoring](https://adventofcode.com/2021/day/10) | [Source](src/main/scala/AdventOfCode2021/Day10.scala) |
| 11 | [Dumbo Octopus](https://adventofcode.com/2021/day/11) | [Source](src/main/scala/AdventOfCode2021/Day11.scala) |
| 12 | [Passage Pathing](https://adventofcode.com/2021/day/12) | [Source](src/main/scala/AdventOfCode2021/Day12.scala) |
| 13 | [Transparent Origami](https://adventofcode.com/2021/day/13) | [Source](src/main/scala/AdventOfCode2021/Day13.scala) |
| 14 | [Extended Polymerization](https://adventofcode.com/2021/day/14) | [Source](src/main/scala/AdventOfCode2021/Day14.scala) |
| 15 | [Chiton](https://adventofcode.com/2021/day/15) | [Source](src/main/scala/AdventOfCode2021/Day15.scala) |
| 16 | [Packet Decoder](https://adventofcode.com/2021/day/16) | [Source](src/main/scala/AdventOfCode2021/Day16.scala) |
| 17 | [Trick Shot](https://adventofcode.com/2021/day/17) | [Source](src/main/scala/AdventOfCode2021/Day17.scala) |
| 18 | [Snailfish](https://adventofcode.com/2021/day/18) | [Source](src/main/scala/AdventOfCode2021/Day18.scala) |
| 19 | [Beacon Scanner](https://adventofcode.com/2021/day/19) | [Source](src/main/scala/AdventOfCode2021/Day19.scala) |
| 20 | [Trench Map](https://adventofcode.com/2021/day/20) | [Source](src/main/scala/AdventOfCode2021/Day20.scala) |
| 21 | [Dirac Dice](https://adventofcode.com/2021/day/21) | [Source](src/main/scala/AdventOfCode2021/Day21.scala) |
| 22 | [Reactor Reboot](https://adventofcode.com/2021/day/22) | [Source](src/main/scala/AdventOfCode2021/Day22.scala) |
| 23 | [Amphipod](https://adventofcode.com/2021/day/23) | [Source](src/main/scala/AdventOfCode2021/Day23.scala) |
| 24 | [Arithmetic Logic Unit](https://adventofcode.com/2021/day/24) | [Source](src/main/scala/AdventOfCode2021/Day24.scala) |
| 25 | [Sea Cucumber](https://adventofcode.com/2021/day/25) | [Source](src/main/scala/AdventOfCode2021/Day25.scala) |

## 2020

| Day | Problem | Solution |
| --- | --- | --- |
| 1 | [Report Repair](https://adventofcode.com/2020/day/1) | [Source](src/main/scala/AdventOfCode2020/Day01.scala) |
| 2 | [Password Philosophy](https://adventofcode.com/2020/day/2) | [Source](src/main/scala/AdventOfCode2020/Day02.scala) |
| 3 | [Toboggan Trajectory](https://adventofcode.com/2020/day/3) | [Source](src/main/scala/AdventOfCode2020/Day03.scala) |
| 4 | [Passport Processing](https://adventofcode.com/2020/day/4) | [Source](src/main/scala/AdventOfCode2020/Day04.scala) |
| 5 | [Binary Boarding](https://adventofcode.com/2020/day/5) | [Source](src/main/scala/AdventOfCode2020/Day05.scala) |
| 6 | [Custom Customs](https://adventofcode.com/2020/day/6) | [Source](src/main/scala/AdventOfCode2020/Day06.scala) |
| 7 | [Handy Haversacks](https://adventofcode.com/2020/day/7) | [Source](src/main/scala/AdventOfCode2020/Day07.scala) |
| 8 | [Handheld Halting](https://adventofcode.com/2020/day/8) | [Source](src/main/scala/AdventOfCode2020/Day08.scala) |
| 9 | [Encoding Error](https://adventofcode.com/2020/day/9) | [Source](src/main/scala/AdventOfCode2020/Day09.scala) |
| 10 | [Adapter Array](https://adventofcode.com/2020/day/10) | [Source](src/main/scala/AdventOfCode2020/Day10.scala) |
| 11 | [Seating System](https://adventofcode.com/2020/day/11) | [Source](src/main/scala/AdventOfCode2020/Day11.scala) |
| 12 | [Rain Risk](https://adventofcode.com/2020/day/12) | [Source](src/main/scala/AdventOfCode2020/Day12.scala) |
| 13 | [Shuttle Search](https://adventofcode.com/2020/day/13) | [Source](src/main/scala/AdventOfCode2020/Day13.scala) |
| 14 | [Docking Data](https://adventofcode.com/2020/day/14) | [Source](src/main/scala/AdventOfCode2020/Day14.scala) |
| 15 | [Rambunctious Recitation](https://adventofcode.com/2020/day/15) | [Source](src/main/scala/AdventOfCode2020/Day15.scala) |
| 16 | [Ticket Translation](https://adventofcode.com/2020/day/16) | [Source](src/main/scala/AdventOfCode2020/Day16.scala) |
| 17 | [Conway Cubes](https://adventofcode.com/2020/day/17) | [Source](src/main/scala/AdventOfCode2020/Day17.scala) |
| 18 | [Operation Order](https://adventofcode.com/2020/day/18) | [Source](src/main/scala/AdventOfCode2020/Day18.scala) |
| 19 | [Monster Messages](https://adventofcode.com/2020/day/19) | [Source](src/main/scala/AdventOfCode2020/Day19.scala) |
| 20 | [Jurassic Jigsaw](https://adventofcode.com/2020/day/20) | [Source](src/main/scala/AdventOfCode2020/Day20.scala) |
| 21 | [Allergen Assessment](https://adventofcode.com/2020/day/21) | [Source](src/main/scala/AdventOfCode2020/Day21.scala) |
| 22 | [Crab Combat](https://adventofcode.com/2020/day/22) | [Source](src/main/scala/AdventOfCode2020/Day22.scala) |
| 23 | [Crab Cups](https://adventofcode.com/2020/day/23) | [Source](src/main/scala/AdventOfCode2020/Day23.scala) |
| 24 | [Lobby Layout](https://adventofcode.com/2020/day/24) | [Source](src/main/scala/AdventOfCode2020/Day24.scala) |
| 25 | [Combo Breaker](https://adventofcode.com/2020/day/25) | [Source](src/main/scala/AdventOfCode2020/Day25.scala) |

## 2019

| Day | Problem | Solution |
| --- | --- | --- |
| 1 | [The Tyranny of the Rocket Equation](https://adventofcode.com/2019/day/1) | [Source](src/main/scala/AdventOfCode2019/Day01.scala) |
| 2 | [1202 Program Alarm](https://adventofcode.com/2019/day/2) | [Source](src/main/scala/AdventOfCode2019/Day02.scala) |
| 3 | [Crossed Wires](https://adventofcode.com/2019/day/3) | [Source](src/main/scala/AdventOfCode2019/Day03.scala) |
| 4 | [Secure Container](https://adventofcode.com/2019/day/4) | [Source](src/main/scala/AdventOfCode2019/Day04.scala) |
| 5 | [Sunny with a Chance of Asteroids](https://adventofcode.com/2019/day/5) | [Source](src/main/scala/AdventOfCode2019/Day05.scala) |
| 6 | [Universal Orbit Map](https://adventofcode.com/2019/day/6) | [Source](src/main/scala/AdventOfCode2019/Day06.scala) |
| 7 | [Amplification Circuit](https://adventofcode.com/2019/day/7) | [Source](src/main/scala/AdventOfCode2019/Day07.scala) |
| 8 | [Space Image Format](https://adventofcode.com/2019/day/8) | [Source](src/main/scala/AdventOfCode2019/Day08.scala) |
| 9 | [Sensor Boost](https://adventofcode.com/2019/day/9) | [Source](src/main/scala/AdventOfCode2019/Day09.scala) |
| 10 | [Monitoring Station](https://adventofcode.com/2019/day/10) | [Source](src/main/scala/AdventOfCode2019/Day10.scala) |
| 11 | [Space Police](https://adventofcode.com/2019/day/11) | [Source](src/main/scala/AdventOfCode2019/Day11.scala) |
| 12 | [The N-Body Problem](https://adventofcode.com/2019/day/12) | [Source](src/main/scala/AdventOfCode2019/Day12.scala) |
| 13 | [Care Package](https://adventofcode.com/2019/day/13) | [Source](src/main/scala/AdventOfCode2019/Day13.scala) |
| 14 | [Space Stoichiometry](https://adventofcode.com/2019/day/14) | [Source](src/main/scala/AdventOfCode2019/Day14.scala) |
| 15 | [Oxygen System](https://adventofcode.com/2019/day/15) | [Source](src/main/scala/AdventOfCode2019/Day15.scala) |
| 16 | [Flawed Frequency Transmission](https://adventofcode.com/2019/day/16) | [Source](src/main/scala/AdventOfCode2019/Day16.scala) |
| 17 | [Set and Forget](https://adventofcode.com/2019/day/17) | [Source](src/main/scala/AdventOfCode2019/Day17.scala) |
| 18 | [Many-Worlds Interpretation](https://adventofcode.com/2019/day/18) | [Source](src/main/scala/AdventOfCode2019/Day18.scala) |
| 19 | [Tractor Beam](https://adventofcode.com/2019/day/19) | [Source](src/main/scala/AdventOfCode2019/Day19.scala) |
| 20 | [Donut Maze](https://adventofcode.com/2019/day/20) | [Source](src/main/scala/AdventOfCode2019/Day20.scala) |
| 21 | [Springdroid Adventure](https://adventofcode.com/2019/day/21) | [Source](src/main/scala/AdventOfCode2019/Day21.scala) |
| 22 | [Slam Shuffle](https://adventofcode.com/2019/day/22) | [Source](src/main/scala/AdventOfCode2019/Day22.scala) |
| 23 | [Category Six](https://adventofcode.com/2019/day/23) | [Source](src/main/scala/AdventOfCode2019/Day23.scala) |
| 24 | [Planet of Discord](https://adventofcode.com/2019/day/24) | [Source](src/main/scala/AdventOfCode2019/Day24.scala) |
| 25 | [Cryostasis](https://adventofcode.com/2019/day/25) | [Source](src/main/scala/AdventOfCode2019/Day25.scala) |

## 2018

| Day | Problem | Solution |
| --- | --- | --- |
| 1 | [Chronal Calibration](https://adventofcode.com/2018/day/1) | [Source](src/main/scala/AdventOfCode2018/Day01.scala) |
| 2 | [Inventory Management System](https://adventofcode.com/2018/day/2) | [Source](src/main/scala/AdventOfCode2018/Day02.scala) |
| 3 | [No Matter How You Slice It](https://adventofcode.com/2018/day/3) | [Source](src/main/scala/AdventOfCode2018/Day03.scala) |
| 4 | [Repose Record](https://adventofcode.com/2018/day/4) | [Source](src/main/scala/AdventOfCode2018/Day04.scala) |
| 5 | [Alchemical Reduction](https://adventofcode.com/2018/day/5) | [Source](src/main/scala/AdventOfCode2018/Day05.scala) |
| 6 | [Chronal Coordinates](https://adventofcode.com/2018/day/6) | [Source](src/main/scala/AdventOfCode2018/Day06.scala) |
| 7 | [The Sum of Its Parts](https://adventofcode.com/2018/day/7) | [Source](src/main/scala/AdventOfCode2018/Day07.scala) |
| 8 | [Memory Maneuver](https://adventofcode.com/2018/day/8) | [Source](src/main/scala/AdventOfCode2018/Day08.scala) |
| 9 | [Marble Mania](https://adventofcode.com/2018/day/9) | [Source](src/main/scala/AdventOfCode2018/Day09.scala) |
| 10 | [The Stars Align](https://adventofcode.com/2018/day/10) | [Source](src/main/scala/AdventOfCode2018/Day10.scala) |
| 11 | [Chronal Charge](https://adventofcode.com/2018/day/11) | [Source](src/main/scala/AdventOfCode2018/Day11.scala) |
| 12 | [Subterranean Sustainability](https://adventofcode.com/2018/day/12) | [Source](src/main/scala/AdventOfCode2018/Day12.scala) |
| 13 | [Mine Cart Madness](https://adventofcode.com/2018/day/13) | [Source](src/main/scala/AdventOfCode2018/Day13.scala) |
| 14 | [Chocolate Charts](https://adventofcode.com/2018/day/14) | [Source](src/main/scala/AdventOfCode2018/Day14.scala) |
| 15 | [Beverage Bandits](https://adventofcode.com/2018/day/15) | [Source](src/main/scala/AdventOfCode2018/Day15.scala) |
| 16 | [Chronal Classification](https://adventofcode.com/2018/day/16) | [Source](src/main/scala/AdventOfCode2018/Day16.scala) |
| 17 | [Reservoir Research](https://adventofcode.com/2018/day/17) | [Source](src/main/scala/AdventOfCode2018/Day17.scala) |
| 18 | [Settlers of The North Pole](https://adventofcode.com/2018/day/18) | [Source](src/main/scala/AdventOfCode2018/Day18.scala) |
| 19 | [Go With The Flow](https://adventofcode.com/2018/day/19) | [Source](src/main/scala/AdventOfCode2018/Day19.scala) |
| 20 | [A Regular Map](https://adventofcode.com/2018/day/20) | [Source](src/main/scala/AdventOfCode2018/Day20.scala) |
| 21 | [Chronal Conversion](https://adventofcode.com/2018/day/21) | [Source](src/main/scala/AdventOfCode2018/Day21.scala) |
| 22 | [Mode Maze](https://adventofcode.com/2018/day/22) | [Source](src/main/scala/AdventOfCode2018/Day22.scala) |
| 23 | [Experimental Emergency Teleportation](https://adventofcode.com/2018/day/23) | [Source](src/main/scala/AdventOfCode2018/Day23.scala) |
| 24 | [Immune System Simulator 20XX](https://adventofcode.com/2018/day/24) | [Source](src/main/scala/AdventOfCode2018/Day24.scala) |
| 25 | [Four-Dimensional Adventure](https://adventofcode.com/2018/day/25) | [Source](src/main/scala/AdventOfCode2018/Day25.scala) |

## 2017

| Day | Problem | Solution |
| --- | --- | --- |
| 1 | [Inverse Captcha](https://adventofcode.com/2017/day/1) | [Source](src/main/scala/AdventOfCode2017/Day01.scala) |
| 2 | [Corruption Checksum](https://adventofcode.com/2017/day/2) | [Source](src/main/scala/AdventOfCode2017/Day02.scala) |
| 3 | [Spiral Memory](https://adventofcode.com/2017/day/3) | [Source](src/main/scala/AdventOfCode2017/Day03.scala) |
| 4 | [High-Entropy Passphrases](https://adventofcode.com/2017/day/4) | [Source](src/main/scala/AdventOfCode2017/Day04.scala) |
| 5 | [A Maze of Twisty Trampolines, All Alike](https://adventofcode.com/2017/day/5) | [Source](src/main/scala/AdventOfCode2017/Day05.scala) |
| 6 | [Memory Reallocation](https://adventofcode.com/2017/day/6) | [Source](src/main/scala/AdventOfCode2017/Day06.scala) |
| 7 | [Recursive Circus](https://adventofcode.com/2017/day/7) | [Source](src/main/scala/AdventOfCode2017/Day07.scala) |
| 8 | [I Heard You Like Registers](https://adventofcode.com/2017/day/8) | [Source](src/main/scala/AdventOfCode2017/Day08.scala) |
| 9 | [Stream Processing](https://adventofcode.com/2017/day/9) | [Source](src/main/scala/AdventOfCode2017/Day09.scala) |
| 10 | [Knot Hash](https://adventofcode.com/2017/day/10) | [Source](src/main/scala/AdventOfCode2017/Day10.scala) |
| 11 | [Hex Ed](https://adventofcode.com/2017/day/11) | [Source](src/main/scala/AdventOfCode2017/Day11.scala) |
| 12 | [Digital Plumber](https://adventofcode.com/2017/day/12) | [Source](src/main/scala/AdventOfCode2017/Day12.scala) |
| 13 | [Packet Scanners](https://adventofcode.com/2017/day/13) | [Source](src/main/scala/AdventOfCode2017/Day13.scala) |
| 14 | [Disk Defragmentation](https://adventofcode.com/2017/day/14) | [Source](src/main/scala/AdventOfCode2017/Day14.scala) |
| 15 | [Dueling Generators](https://adventofcode.com/2017/day/15) | [Source](src/main/scala/AdventOfCode2017/Day15.scala) |
| 16 | [Permutation Promenade](https://adventofcode.com/2017/day/16) | [Source](src/main/scala/AdventOfCode2017/Day16.scala) |
| 17 | [Spinlock](https://adventofcode.com/2017/day/17) | [Source](src/main/scala/AdventOfCode2017/Day17.scala) |
| 18 | [Duet](https://adventofcode.com/2017/day/18) | [Source](src/main/scala/AdventOfCode2017/Day18.scala) |
| 19 | [A Series of Tubes](https://adventofcode.com/2017/day/19) | [Source](src/main/scala/AdventOfCode2017/Day19.scala) |
| 20 | [Particle Swarm](https://adventofcode.com/2017/day/20) | [Source](src/main/scala/AdventOfCode2017/Day20.scala) |
| 21 | [Fractal Art](https://adventofcode.com/2017/day/21) | [Source](src/main/scala/AdventOfCode2017/Day21.scala) |
| 22 | [Sporifica Virus](https://adventofcode.com/2017/day/22) | [Source](src/main/scala/AdventOfCode2017/Day22.scala) |
| 23 | [Coprocessor Conflagration](https://adventofcode.com/2017/day/23) | [Source](src/main/scala/AdventOfCode2017/Day23.scala) |
| 24 | [Electromagnetic Moat](https://adventofcode.com/2017/day/24) | [Source](src/main/scala/AdventOfCode2017/Day24.scala) |
| 25 | [The Halting Problem](https://adventofcode.com/2017/day/25) | [Source](src/main/scala/AdventOfCode2017/Day25.scala) |

## 2016

| Day | Problem | Solution |
| --- | --- | --- |
| 1 | [No Time for a Taxicab](https://adventofcode.com/2016/day/1) | [Source](src/main/scala/AdventOfCode2016/Day01.scala) |
| 2 | [Bathroom Security](https://adventofcode.com/2016/day/2) | [Source](src/main/scala/AdventOfCode2016/Day02.scala) |
| 3 | [Squares With Three Sides](https://adventofcode.com/2016/day/3) | [Source](src/main/scala/AdventOfCode2016/Day03.scala) |
| 4 | [Security Through Obscurity](https://adventofcode.com/2016/day/4) | [Source](src/main/scala/AdventOfCode2016/Day04.scala) |
| 5 | [How About a Nice Game of Chess?](https://adventofcode.com/2016/day/5) | [Source](src/main/scala/AdventOfCode2016/Day05.scala) |
| 6 | [Signals and Noise](https://adventofcode.com/2016/day/6) | [Source](src/main/scala/AdventOfCode2016/Day06.scala) |
| 7 | [Internet Protocol Version 7](https://adventofcode.com/2016/day/7) | [Source](src/main/scala/AdventOfCode2016/Day07.scala) |
| 8 | [Two-Factor Authentication](https://adventofcode.com/2016/day/8) | [Source](src/main/scala/AdventOfCode2016/Day08.scala) |
| 9 | [Explosives in Cyberspace](https://adventofcode.com/2016/day/9) | [Source](src/main/scala/AdventOfCode2016/Day09.scala) |
| 10 | [Balance Bots](https://adventofcode.com/2016/day/10) | [Source](src/main/scala/AdventOfCode2016/Day10.scala) |
| 11 | [Radioisotope Thermoelectric Generators](https://adventofcode.com/2016/day/11) | [Source](src/main/scala/AdventOfCode2016/Day11.scala) |
| 12 | [Leonardo's Monorail](https://adventofcode.com/2016/day/12) | [Source](src/main/scala/AdventOfCode2016/Day12.scala) |
| 13 | [A Maze of Twisty Little Cubicles](https://adventofcode.com/2016/day/13) | [Source](src/main/scala/AdventOfCode2016/Day13.scala) |
| 14 | [One-Time Pad](https://adventofcode.com/2016/day/14) | [Source](src/main/scala/AdventOfCode2016/Day14.scala) |
| 15 | [Timing is Everything](https://adventofcode.com/2016/day/15) | [Source](src/main/scala/AdventOfCode2016/Day15.scala) |
| 16 | [Dragon Checksum](https://adventofcode.com/2016/day/16) | [Source](src/main/scala/AdventOfCode2016/Day16.scala) |
| 17 | [Two Steps Forward](https://adventofcode.com/2016/day/17) | [Source](src/main/scala/AdventOfCode2016/Day17.scala) |
| 18 | [Like a Rogue](https://adventofcode.com/2016/day/18) | [Source](src/main/scala/AdventOfCode2016/Day18.scala) |
| 19 | [An Elephant Named Joseph](https://adventofcode.com/2016/day/19) | [Source](src/main/scala/AdventOfCode2016/Day19.scala) |
| 20 | [Firewall Rules](https://adventofcode.com/2016/day/20) | [Source](src/main/scala/AdventOfCode2016/Day20.scala) |
| 21 | [Scrambled Letters and Hash](https://adventofcode.com/2016/day/21) | [Source](src/main/scala/AdventOfCode2016/Day21.scala) |
| 22 | [Grid Computing](https://adventofcode.com/2016/day/22) | [Source](src/main/scala/AdventOfCode2016/Day22.scala) |
| 23 | [Safe Cracking](https://adventofcode.com/2016/day/23) | [Source](src/main/scala/AdventOfCode2016/Day23.scala) |
| 24 | [Air Duct Spelunking](https://adventofcode.com/2016/day/24) | [Source](src/main/scala/AdventOfCode2016/Day24.scala) |
| 25 | [Clock Signal](https://adventofcode.com/2016/day/25) | [Source](src/main/scala/AdventOfCode2016/Day25.scala) |

## 2015

| Day | Problem | Solution |
| --- | --- | --- |
| 1 | [Not Quite Lisp](https://adventofcode.com/2015/day/1) | [Source](src/main/scala/AdventOfCode2015/Day01.scala) |
| 2 | [I Was Told There Would Be No Math](https://adventofcode.com/2015/day/2) | [Source](src/main/scala/AdventOfCode2015/Day02.scala) |
| 3 | [Perfectly Spherical Houses in a Vacuum](https://adventofcode.com/2015/day/3) | [Source](src/main/scala/AdventOfCode2015/Day03.scala) |
| 4 | [The Ideal Stocking Stuffer](https://adventofcode.com/2015/day/4) | [Source](src/main/scala/AdventOfCode2015/Day04.scala) |
