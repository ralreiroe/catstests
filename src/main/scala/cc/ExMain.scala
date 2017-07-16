package cc

object ExMain extends App {

  val (size, numShifts) = scala.io.StdIn.readLine().split(" ").map(_.toInt).toList match {
    case List(a, b) => (a, b)
  }

  println(size)
  println(numShifts)
}
