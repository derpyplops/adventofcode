import scala.io.Source

val filename = "002.in"
val lines = Source.fromFile(filename).getLines.toList

case class Problem(min: Int, max: Int, letter: Char, password: String) {
  def solve: Boolean = {
    val c = password.count(_ == letter)
    c >= min && c <= max
  }
}

def parse(s: String): Problem = {
  val lst = s.split(" ")
  val minmax = lst.head.split("-").map(_.toInt)
  val letter = lst(1)(0)
  val password = lst(2)
  Problem(minmax(0), minmax(1), letter, password)
}


val solved = lines.map(parse).map(_.solve).count(_ == true)
print(solved)