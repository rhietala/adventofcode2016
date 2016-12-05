package adventofcode2016

object Day01 {
  object Direction extends Enumeration {
    type Direction = Value
    val North = Value("North")
    val South = Value("South")
    val East = Value("East")
    val West = Value("West")
  }

  object TurnDirection extends Enumeration {
    type TurnDirection = Value
    val Right = Value("Right")
    val Left = Value("Left")
  }

  import Direction._
  import TurnDirection._

  type ParsedDirection = (TurnDirection, Int)
  type Coordinates = (Int, Int)

  def length(c: Coordinates): Int = {
    scala.math.abs(c._1) + scala.math.abs(c._2)
  }

  def parseDirection(s: String): TurnDirection = s.head match {
    case 'R' => Right
    case 'L' => Left
    case _ => throw new IllegalArgumentException("unknown direction " + s);
  }

  def changeDirection(current: Direction, turnTo: TurnDirection):
      Direction = (current, turnTo) match {
    case (North, Right) => East
    case (North, Left) => West
    case (South, Right) => West
    case (South, Left) => East
    case (East, Right) => South
    case (East, Left) => North
    case (West, Right) => North
    case (West, Left) => South
  }

  def parseLength(s: String): Int = {
    s.tail.toInt
  }

  def parse(s: String): ParsedDirection = {
    (parseDirection(s), parseLength(s))
  }

  def add(start: Coordinates, dir: Direction, length: Int):
      Array[Coordinates] = dir match {
    case North =>
      Range(start._2, start._2 + length, +1).toArray.map(y => (start._1, y + 1))
    case South =>
      Range(start._2, start._2 - length, -1).toArray.map(y => (start._1, y - 1))
    case East =>
      Range(start._1, start._1 + length, +1).toArray.map(x => (x + 1, start._2))
    case West =>
      Range(start._1, start._1 - length, -1).toArray.map(x => (x - 1, start._2))
  }

  def walk1(
    startDir: Direction,
    startCoords: Coordinates,
    direction: ParsedDirection): (Direction, Array[Coordinates]) = {
    val newDir: Direction = changeDirection(startDir, direction._1)
    (newDir, add(startCoords, newDir, direction._2))
  }

  def walkRecur(
    dir: Direction,
    path: Array[Coordinates],
    dirs: Array[ParsedDirection]
  ): Array[Coordinates] = {
    if (dirs.isEmpty) path
    else {
      val x = walk1(dir, path.last, dirs.head)
      walkRecur(x._1, path ++ x._2, dirs.tail)
    }
  }

  def walk(s: String): Array[Coordinates] = {
    walkRecur(
      North,
      Array((0,0)),
      s.split(',').map(s2 => parse(s2.trim)))
  }

  def lengthToLast(s: String): Int = {
    length(walk(s).last)
  }

  def lengthToFirstVisitedTwice(s: String): Int = {
    val path = walk(s)
    val duplicates = path.diff(path.distinct).distinct
    val firstDuplicate = path.find(c => duplicates.contains(c))
    firstDuplicate match {
      case Some(x) => length(x)
      case None => 0
    }
  }

  def assignment1(): Int = {
    lengthToLast(io.Source.fromFile("data/day01.txt").mkString)
  }

  def assignment2(): Int = {
    lengthToFirstVisitedTwice(io.Source.fromFile("data/day01.txt").mkString)
  }

  def main(args: Array[String]): Unit = {
    println("Day01 assignment 1: " + assignment1())
    println("Day01 assignment 2: " + assignment2())
  }
}
