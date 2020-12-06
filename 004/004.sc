import scala.collection.mutable.ArrayBuffer
import scala.io.Source

val filename = "004.in"
val lines = Source.fromFile(filename).getLines.toArray
val buf = ArrayBuffer(lines: _*)

def seqSplitter[T](s: ArrayBuffer[T], delimiter : T) =
  (0 +: s.indices.filter(s(_)==delimiter) :+ s.size)  //find split locations
    .sliding(2)
    .map(idx => s.slice(idx.head, idx.last)) //extract the slice
    .dropWhile(_.isEmpty) //take care of the first element
    .toList

val passportStrings = seqSplitter(buf, "").tail.map(_.tail).map(_.reduce(_ + " " + _))

val passwordRegex = "(.+):(.+)".r

def parse(str: String): String = str match {
  case passwordRegex(code, _) => code
}

val validCodes = Set(
  "byr",
  "iyr",
  "eyr",
  "hgt",
  "hcl",
  "ecl",
  "pid",
)

println(passportStrings.map(_.split(" ").toSet).map(_.map(parse)).map(validCodes.subsetOf(_)).count(_ == true))