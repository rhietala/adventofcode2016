package adventofcode2016

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Day03Suite extends FunSuite {

  import Day03._

  test("part 1, count number of correct triangles, 2 pos 1 neg") {
    val input = "2 3 4\n1 2 3\n4 5 6"
    assert(count(parse1(input)) == 2)
  }

  test("part 2, count number of correct triangles, 1 pos 2 neg") {
    val input = "2 3 4\n1 2 3\n4 5 6"
    assert(count(parse2(input)) == 1)
  }
}
