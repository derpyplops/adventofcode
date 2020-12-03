import scala.#::
import scala.io.Source

val filename = "003.in"
val lines = Source.fromFile(filename).getLines.toArray.map(_.toCharArray)

val maxlen = lines.head.length
val height = lines.length

case class Slope(i: Int, j: Int)
val slopes = List(Slope(1,1), Slope(3,1), Slope(5,1), Slope(7,1), Slope(1,2))

case class Position(i: Int, j: Int) {
  def next(slope: Slope) = Position((i + slope.i) % maxlen, j + slope.j)
  def isTree = lines(j)(i) == '#'
}

val init = Position(0,0)

def positionStream(slope: Slope): Iterator[Position] = Iterator.iterate(init)(_.next(slope))

println(positionStream(Slope(1,3)).takeWhile(_.j < height).count(_.isTree))
println(slopes.map(slope => positionStream(slope).takeWhile(_.j < height).count(_.isTree)).map(_.toLong).product)
