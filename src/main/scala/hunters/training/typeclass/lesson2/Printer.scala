package hunters.training.typeclass.lesson2

trait Printer[T] {
  def print(t: T): String
}
object Printer {

  implicit val stringIdentityPrinter: Printer[String] = identity[String]

  implicit val intPrinter: Printer[Int] = String.valueOf

  implicit def list[A : Printer]: Printer[List[A]] = ???

  implicit def either[L : Printer, R : Printer]: Printer[Either[L, R]] = ???
}
