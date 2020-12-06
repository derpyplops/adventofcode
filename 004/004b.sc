import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.matching.Regex

val filename = "004.in"
val lines = Source.fromFile(filename).getLines.toArray
val buf = ArrayBuffer(lines: _*)

def seqSplitter[T](s: ArrayBuffer[T], delimiter : T) =
  (0 +: s.indices.filter(s(_)==delimiter) :+ s.size)  //find split locations
    .sliding(2)
    .map(idx => s.slice(idx.head, idx.last)) //extract the slice
    .dropWhile(_.isEmpty) //take care of the first element
    .toList


val passportStrings = seqSplitter(buf, "").tail.map(_.tail).filter(_.nonEmpty).map(_.reduce(_ + " " + _))

val passwordRegex = "(.+):(.+)".r

val codes = Set[String](
  "byr",
  "iyr",
  "eyr",
  "hgt",
  "hcl",
  "ecl",
  "pid",
)

val byrRegex = """byr:(\d{4})""".r
val iyrRegex = """iyr:(\d{4})""".r
val eyrRegex = """eyr:(\d{4})""".r
val hgtRegex = """hgt:(\d+)(cm|in)""".r
val hclRegex = """hcl:#[0-9a-f]{6}""".r
val eclRegex = """ecl:(amb)|(blu)|(brn)|(gry)|(grn)|(hzl)|(oth)""".r
val pidRegex = """pid:\d{9}""".r

def validate(str: String): Boolean = str match {
  case byrRegex(year) if (year.toInt >= 1920 && year.toInt <= 2002) => true
  case iyrRegex(year) if (year.toInt >= 2010 && year.toInt <= 2020) => true
  case eyrRegex(year) if (year.toInt >= 2020 && year.toInt <= 2030) => true
  case hgtRegex(h, unit) if ((unit == "in" && h.toInt >= 59 && h.toInt <= 76) || (unit == "cm" && h.toInt >= 150 && h.toInt <= 193)) => true
  case hclRegex(_*) => true
  case eclRegex(_*) => true
  case pidRegex(_*) => true
  case _ => false
}

println(
  passportStrings.map(_.split(" ").toSet)
    .map(_.map(s => (s.split(":").head, validate(s))))
//      .map(
//        pairSet =>
//          pairSet.forall(_._2 == true) && (codes -- pairSet.map(_._1)).isEmpty
//      )
//    .count(_ == true)
)