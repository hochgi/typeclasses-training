package hunters.training.typeclass.lesson3

trait TreePrinter[T] {

  /**
   * To make [[TreePrinter]]s more composable,
   * instead of [[String]], we return a [[TreePrinter.Tree]] whose value is always just a list of lines.
   * This way, we can compose complex structures tree representations more easily.
   *
   * @param t Something that can be made to a tree
   * @return Tree
   */
  def print(t: T): TreePrinter.Tree
}
object TreePrinter {

  case class Tree(valueLines: List[String], children: List[Tree])

  def printTree[T : TreePrinter](t: T): String = ???


  implicit val intsTreePrinter: TreePrinter[Int] = ???

  implicit val stringsTreePrinter: TreePrinter[String] = ???

  implicit def tuple2TreePrinter[A : TreePrinter, B : TreePrinter]: TreePrinter[(A, B)] = ???
}
