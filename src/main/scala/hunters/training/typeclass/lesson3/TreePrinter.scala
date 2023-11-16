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

  def printTree[T : TreePrinter](t: T): String = {

    def toLines(subtree: Tree): List[String] = subtree match {
      case Tree(lines, Nil) => lines
      case Tree(lines, nonEmptyChildren) =>
        val last = toLines(nonEmptyChildren.last)
        val prefixedLast = ("└── " + last.head) :: last.tail.map("    ".+)
        val prefixedInit = nonEmptyChildren.init.map { t =>
          val notTheLast = toLines(t)
          ("├── " + notTheLast.head) :: notTheLast.tail.map("│   ".+)
        }

        lines ::: prefixedInit.foldRight(prefixedLast)(_ ::: _)
    }

    toLines(implicitly[TreePrinter[T]].print(t)).mkString("\n")
  }


  implicit val intsTreePrinter: TreePrinter[Int] = (i: Int) => Tree(String.valueOf(i) :: Nil, Nil)

  implicit val stringsTreePrinter: TreePrinter[String] = (s: String) => Tree(s.linesIterator.toList, Nil)

  implicit def tuple2TreePrinter[A : TreePrinter, B : TreePrinter]: TreePrinter[(A, B)] = {
    val tupleValueLines: List[String] = List(classOf[(_, _)].getSimpleName)

    { case (a, b) =>
      Tree(tupleValueLines, List(implicitly[TreePrinter[A]].print(a), implicitly[TreePrinter[B]].print(b)))
    }
  }
}
