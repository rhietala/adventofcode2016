package adventofcode2016

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Day04Suite extends FunSuite {

  import Day04._

  test("part 1, ex1") {
    val input = "aaaaa-bbb-z-y-x-123[abxyz]"
    assert(isRealRoom(input) == true)
  }

  test("part 1, ex2") {
    val input = "a-b-c-d-e-f-g-h-987[abcde]"
    assert(isRealRoom(input) == true)
  }

  test("part 1, ex3") {
    val input = "not-a-real-room-404[oarel]"
    assert(isRealRoom(input) == true)
  }

  test("part 1, ex4") {
    val input = "totally-real-room-200[decoy]"
    assert(isRealRoom(input) == false)
  }

  test("part 1, sum of sector ids") {
    val input = "aaaaa-bbb-z-y-x-123[abxyz]\n" +
    "a-b-c-d-e-f-g-h-987[abcde]\n" +
    "not-a-real-room-404[oarel]\n" +
    "totally-real-room-200[decoy]"
    assert(sumOfSectorIds(input) == 1514)
  }

  test("part 2, decrypt") {
    val input = "qzmt-zixmtkozy-ivhz-343[abcde]"
    assert(decrypt(input) == "very encrypted name")
  }
}
