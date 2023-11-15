package hunters.training.typeclass.lesson2

trait Printer[T] {
  def print(t: T): String
}
object Printer {

  implicit val stringIdentityPrinter: Printer[String] = identity[String]

  implicit val intPrinter: Printer[Int] = String.valueOf

  implicit def list[A : Printer]: Printer[List[A]] = {
    val aPrinter = implicitly[Printer[A]]
    (list: List[A]) => list.map(aPrinter.print).mkString("\n")
  }

  implicit def either[L : Printer, R : Printer]: Printer[Either[L, R]] = {
    val lPrinter = implicitly[Printer[L]]
    val rPrinter = implicitly[Printer[R]]
    (either: Either[L, R]) => either.fold(lPrinter.print, rPrinter.print)
  }
}
