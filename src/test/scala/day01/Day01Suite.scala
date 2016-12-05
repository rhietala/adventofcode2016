package adventofcode2016

import org.scalatest.FunSuite

class Day01Suite extends FunSuite {

  import Day01._
  import Direction._
  import TurnDirection._

  test("parseDirection, right") {
    assert(parseDirection("R2") == Right)
  }

  test("parseDirection, left") {
    assert(parseDirection("L5") == Left)
  }

  test("changeDirection, turn right") {
    assert(changeDirection(North, Right) == East)
  }

  test("changeDirection, turn left") {
    assert(changeDirection(West, Left) == South)
  }

  test("parseLength, 2") {
    assert(parseLength("R2") == 2)
  }

  test("parse, L3") {
    assert(parse("L3") == (Left, 3))
  }

  test("add, north") {
    assert(add((0,0), North, 2) == (0,2))
  }

  test("add, west") {
    assert(add((15,13), West, 20) == (-5, 13))
  }

  test("walk1, 5 east from start") {
    assert(walk1(North, (0,0), (Right, 5)) == (East, (5, 0)))
  }

  test("R2 L3 = 5") {
    assert(Day01.walk("R2, L3") == 5)
  }

  test("R2, R2, R2 = 2") {
    assert(Day01.walk("R2, R2, R2") == 2)
  }

  test("R5, L5, R5, R3 = 12") {
    assert(Day01.walk("R5, L5, R5, R3") == 12)
  }
}
