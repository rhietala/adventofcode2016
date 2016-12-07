package adventofcode2016

object Day03 {

  def isTriangle(sides: Array[Int]): Boolean = {
    sides(0) + sides(1) > sides(2)
  }

  def isAnyTriangle(sides: Array[Int]): Boolean = {
    // it would be necessary to calculate only three possible
    // cases: A + B > C, A + C > B, B + C > A, but as it can be
    // done with permutations and three extra calculations..

    sides.permutations.map(isTriangle(_)).reduce(_ && _)
  }

  def count(is: Array[Array[Int]]): Int = {
    is.map(isAnyTriangle(_)).filter(_ == true).size
  }

  def parse1(s: String): Array[Array[Int]] = {
    s.split('\n').map(_.trim.split(" +").map(_.toInt))
  }

  def parse2(s: String): Array[Array[Int]] = {
    s
      .split('\n')
      .map(_.trim.split(" +").map(_.toInt))
      .grouped(3)
      .toArray
      .map(xs => Array(
        Array(xs(0)(0), xs(1)(0), xs(2)(0)),
        Array(xs(0)(1), xs(1)(1), xs(2)(1)),
        Array(xs(0)(2), xs(1)(2), xs(2)(2))
      ))
      .flatten
  }

  def assignment1(): Int = {
    count(parse1(io.Source.fromFile("data/day03.txt").mkString))
  }

  def assignment2(): Int = {
    count(parse2(io.Source.fromFile("data/day03.txt").mkString))
  }

  def main(args: Array[String]): Unit = {
    println("Day03 assignment 1: " + assignment1())
    println("Day03 assignment 2: " + assignment2())
  }

}
