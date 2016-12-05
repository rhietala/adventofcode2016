package adventofcode2016

import org.scalatest.FunSuite
import org.scalatest.Matchers._

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
    add((0,0), North, 2) should equal (Array((0,1), (0,2)))
  }

  test("add, west") {
    add((3, 13), West, 5) should equal (Array((2, 13), (1, 13), (0, 13), (-1, 13), (-2, 13)))
  }

  test("walk1, 5 east from start") {
    walk1(North, (0,0), (Right, 5))._1 should equal (East)
    walk1(North, (0,0), (Right, 5))._2 should equal (Array((1, 0), (2, 0), (3, 0), (4, 0), (5, 0)))
  }

  test("R2 L3 = 5") {
    assert(lengthToLast("R2, L3") == 5)
  }

  test("R2, R2, R2 = 2") {
    assert(lengthToLast("R2, R2, R2") == 2)
  }

  test("R5, L5, R5, R3 = 12") {
    assert(lengthToLast("R5, L5, R5, R3") == 12)
  }

  test("first visited twice R8, R4, R4, R8 = 4") {
    assert(lengthToFirstVisitedTwice("R8, R4, R4, R8") == 4)
  }
}
