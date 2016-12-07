package adventofcode2016

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Day02Suite extends FunSuite {

  import Day02._

  test("move first part with test input") {
    val input = "ULL\nRRDDD\nLURDL\nUUUUD"
    assert(move1(parse(input)) == "1985")
  }

  test("move second part with test input") {
    val input = "ULL\nRRDDD\nLURDL\nUUUUD"
    assert(move2(parse(input)) == "5DB3")
  }
}
