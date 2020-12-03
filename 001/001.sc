import scala.io.Source

def toInt(s: String):Option[Int] = {
    try {
        Some(s.toInt)
    } catch {
        case e: NumberFormatException => None
    }
}

val filename = "001.in"
val lines = Source.fromFile(filename).getLines.toList
val nums = lines.flatMap(toInt)

def f(nums: List[Int]): Int = {
    for {
      i <- nums
      j <- nums.tail
      k <- nums.tail.tail if i + j + k == 2020
    } yield {
      i * j * k
    }
  }.head

println(f(nums))