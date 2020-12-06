import scala.collection.mutable.ArrayBuffer
import scala.io.Source

val filename = "005.in"
val lines = Source.fromFile(filename).getLines.toArray

case class Seating(row: Int, col: Int) {

  def id = row * 8 + col
}

object Seating {
  def parse(s: String) = {
    val (rawRow, rawCol) = s.splitAt(7)
    val row = Integer.parseInt(rawRow.replace('F', '0').replace('B', '1'),2)
    val col = Integer.parseInt(rawCol.replace('L', '0').replace('R', '1'),2)
    Seating(row, col)
  }
}

val res = (28 to 842).toSet -- lines.map(Seating.parse).map(_.id).toList.sorted.toSet
println(res)