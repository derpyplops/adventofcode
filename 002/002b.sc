import scala.io.Source

val filename = "002.in"
val lines = Source.fromFile(filename).getLines.toList

case class Problem(_1: Int, _2: Int, letter: Char, password: String) {
  def solve: Boolean = {
    password(_1) == letter ^ password(_2) == letter
  }
}

def parse(s: String): Problem = {
  val lst = s.split(" ")
  val minmax = lst.head.split("-").map(_.toInt)
  val letter = lst(1)(0)
  val password = lst(2)
  Problem(minmax(0)-1, minmax(1)-1, letter, password)
}


val solved = lines.map(parse andThen(_.solve)).count(_ == true)
print(solved)