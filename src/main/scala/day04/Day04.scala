package adventofcode2016

object Day04 {

  type Checksum = String
  type EncryptedRoomName = String
  type DecryptedRoomName = String
  type ParsedRoom = Tuple3[EncryptedRoomName, Int, Checksum]

  def parse(s: String): ParsedRoom = {
    val roomRegex = """(.+)-(\d+)\[(.+)\]""".r
    s match {
      case roomRegex(n, s, c) => (n, s.toInt, c)
      case _ => throw new IllegalArgumentException("cannot parse room: " + s)
    }
  }

  def calculateChecksum(name: EncryptedRoomName): Checksum = {
    name
      .replaceAll("-", "")
      .toArray
      .groupBy(x => identity(x))
      .map { case (k, v) => (k, v.size) }
      .toSeq
      .sortWith((a, b) => if(a._2 == b._2) a._1 < b._1 else a._2 > b._2)
      .map(_._1)
      .take(5)
      .mkString
  }

  def isRealRoom(room: ParsedRoom): Boolean = {
    val (name, sectorId, checksum) = room
    checksum == calculateChecksum(name)
  }

  def isRealRoom(s: String): Boolean = {
    isRealRoom(parse(s))
  }

  def sumOfSectorIds(s: String): Int = {
    s
      .split('\n')
      .map(parse(_))
      .filter(isRealRoom(_))
      .map(_._2)
      .reduce(_ + _)
  }

  def decrypt(room: ParsedRoom): DecryptedRoomName = {
    val (name, sectorId, checksum) = room
    name
      .split("-")
      .map(s => s
        .map(c => ((c - 'a' + sectorId) % ('z' - 'a' + 1) + 'a').toChar)
        .mkString)
      .mkString(" ")
  }

  def decrypt(s: String): DecryptedRoomName = {
    decrypt(parse(s))
  }

  def assignment1(): Int = {
    sumOfSectorIds(io.Source.fromFile("data/day04.txt").mkString)
  }

  def assignment2(): Int = {
    io.Source.fromFile("data/day04.txt")
      .mkString
      .split('\n')
      .map(parse(_))
      .filter(isRealRoom(_))
      .map(room => (decrypt(room), room._2))
      .filter(room => room._1 == "northpole object storage")
      .head
      ._2
  }

  def main(args: Array[String]): Unit = {
    println("Day04 assignment 1: " + assignment1())
    println("Day04 assignment 2: " + assignment2())
  }

}
