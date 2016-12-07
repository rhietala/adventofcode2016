package adventofcode2016

object Day02 {
  object Direction extends Enumeration {
    type Direction = Value
    val Up = Value("Up")
    val Down = Value("Down")
    val Left = Value("Left")
    val Right = Value("Right")
  }

  import Direction._

  def parseChar(c: Char): Direction = c match {
    case 'U' => Up
    case 'D' => Down
    case 'L' => Left
    case 'R' => Right
    case _ => throw new IllegalArgumentException("unknown direction " + c);
  }

  def parseLine(s: String): Array[Direction] = {
    s.map(c => parseChar(c)).toArray
  }

  def parse(s: String): Array[Array[Direction]] = {
    s.split('\n').map(parseLine(_))
  }

  def moveOne1(pos: Char, dir: Direction): Char = (pos, dir) match {
    case ('1', Up)    => '1'
    case ('1', Down)  => '4'
    case ('1', Left)  => '1'
    case ('1', Right) => '2'
    case ('2', Up)    => '2'
    case ('2', Down)  => '5'
    case ('2', Left)  => '1'
    case ('2', Right) => '3'
    case ('3', Up)    => '3'
    case ('3', Down)  => '6'
    case ('3', Left)  => '2'
    case ('3', Right) => '3'
    case ('4', Up)    => '1'
    case ('4', Down)  => '7'
    case ('4', Left)  => '4'
    case ('4', Right) => '5'
    case ('5', Up)    => '2'
    case ('5', Down)  => '8'
    case ('5', Left)  => '4'
    case ('5', Right) => '6'
    case ('6', Up)    => '3'
    case ('6', Down)  => '9'
    case ('6', Left)  => '5'
    case ('6', Right) => '6'
    case ('7', Up)    => '4'
    case ('7', Down)  => '7'
    case ('7', Left)  => '7'
    case ('7', Right) => '8'
    case ('8', Up)    => '5'
    case ('8', Down)  => '8'
    case ('8', Left)  => '7'
    case ('8', Right) => '9'
    case ('9', Up)    => '6'
    case ('9', Down)  => '9'
    case ('9', Left)  => '8'
    case ('9', Right) => '9'
  }

  def moveOne2(pos: Char, dir: Direction): Char = (pos, dir) match {
    case ('1', Up)    => '1'
    case ('1', Down)  => '3'
    case ('1', Left)  => '1'
    case ('1', Right) => '1'
    case ('2', Up)    => '2'
    case ('2', Down)  => '6'
    case ('2', Left)  => '2'
    case ('2', Right) => '3'
    case ('3', Up)    => '1'
    case ('3', Down)  => '7'
    case ('3', Left)  => '2'
    case ('3', Right) => '4'
    case ('4', Up)    => '4'
    case ('4', Down)  => '8'
    case ('4', Left)  => '3'
    case ('4', Right) => '4'
    case ('5', Up)    => '5'
    case ('5', Down)  => '5'
    case ('5', Left)  => '5'
    case ('5', Right) => '6'
    case ('6', Up)    => '2'
    case ('6', Down)  => 'A'
    case ('6', Left)  => '5'
    case ('6', Right) => '7'
    case ('7', Up)    => '3'
    case ('7', Down)  => 'B'
    case ('7', Left)  => '6'
    case ('7', Right) => '8'
    case ('8', Up)    => '4'
    case ('8', Down)  => 'C'
    case ('8', Left)  => '7'
    case ('8', Right) => '9'
    case ('9', Up)    => '9'
    case ('9', Down)  => '9'
    case ('9', Left)  => '8'
    case ('9', Right) => '9'
    case ('A', Up)    => '6'
    case ('A', Down)  => 'A'
    case ('A', Left)  => 'A'
    case ('A', Right) => 'B'
    case ('B', Up)    => '7'
    case ('B', Down)  => 'D'
    case ('B', Left)  => 'A'
    case ('B', Right) => 'C'
    case ('C', Up)    => '8'
    case ('C', Down)  => 'C'
    case ('C', Left)  => 'B'
    case ('C', Right) => 'C'
    case ('D', Up)    => 'B'
    case ('D', Down)  => 'D'
    case ('D', Left)  => 'D'
    case ('D', Right) => 'D'
  }

  def moveLine1(pos: Char, dirs: Array[Direction]): Char = {
    dirs.foldLeft(pos)(moveOne1(_, _))
  }

  def moveLine2(pos: Char, dirs: Array[Direction]): Char = {
    dirs.foldLeft(pos)(moveOne2(_, _))
  }

  def move1(moves: Array[Array[Direction]]): String = {
    Range(0, moves.size)
      .toArray
      .map(x => Range(0, x+1).toArray.map(i => moves(i)).reduce(_ ++ _))
      .map(moveLine1('5', _))
      .mkString
  }

  def move2(moves: Array[Array[Direction]]): String = {
    Range(0, moves.size)
      .toArray
      .map(x => Range(0, x+1).toArray.map(i => moves(i)).reduce(_ ++ _))
      .map(moveLine2('5', _))
      .mkString
  }

  def assignment1(): String = {
    move1(parse(io.Source.fromFile("data/day02.txt").mkString))
  }

  def assignment2(): String = {
    move2(parse(io.Source.fromFile("data/day02.txt").mkString))
  }

  def main(args: Array[String]): Unit = {
    println("Day02 assignment 1: " + assignment1())
    println("Day02 assignment 2: " + assignment2())
  }

}
