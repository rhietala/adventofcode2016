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
      Coordinates = dir match {
    case North => (start._1, start._2 + length)
    case South => (start._1, start._2 - length)
    case East => (start._1 + length, start._2)
    case West => (start._1 - length, start._2)
  }

  def walk1(
    startDir: Direction,
    startCoords: Coordinates,
    direction: ParsedDirection): (Direction, Coordinates) = {
    val newDir: Direction = changeDirection(startDir, direction._1)
    (newDir, add(startCoords, newDir, direction._2))
  }

  def walkRecur(
    dir: Direction,
    coords: Coordinates,
    dirs: Array[ParsedDirection]
  ): Coordinates = {
    if (dirs.isEmpty) coords
    else {
      val x = walk1(dir, coords, dirs.head)
      walkRecur(x._1, x._2, dirs.tail)
    }
  }

  def walk(s: String): Int = {
    length(
      walkRecur(
        North,
        (0,0),
        s.split(',').map(s2 => parse(s2.trim))))
  }

  def assignment1(): Int = {
    walk(io.Source.fromFile("data/day01.txt").mkString)
  }

  def main(args: Array[String]): Unit = {
    println("Moi")
  }
}
