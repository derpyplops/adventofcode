import scala.collection.mutable.ArrayBuffer
import scala.io.Source

val filename = "006.in"
val lines = Source.fromFile(filename).getLines.toList
val buf = seqSplitter(lines, "")
val inputs = buf.head :: buf.tail.map(_.tail)

def seqSplitter[T](s: List[T], delimiter : T) =
  (0 +: s.indices.filter(s(_)==delimiter) :+ s.size)  //find split locations
    .sliding(2)
    .map(idx => s.slice(idx.head, idx.last)) //extract the slice
    .dropWhile(_.isEmpty) //take care of the first element
    .toList

println(
  inputs
  .map(_.fold("")(_+_))
  .map(_.toSet.size)
  .sum
)

println(
  inputs
    .map(_.map(_.toSet))
    .map(_.reduce(_ intersect _))
    .map(_.size)
    .sum
)