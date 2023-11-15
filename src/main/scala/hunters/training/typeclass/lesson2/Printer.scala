package hunters.training.typeclass.lesson2

trait Printer[T] {
  def print(t: T): String
}
object Printer {

  implicit val stringIdentityPrinter: Printer[String] = ???

  implicit val intPrinter: Printer[Int] = ???
}
