package hunters.training.typeclass.lesson3


/**
 * To make [[TreePrinter]]s even more composable and simple,
 * instead of forcing library users to know (and use) [[TreePrinter.Tree]],
 * we can add composability to our typeclass, and only keeping the tree internally.
 * This way, we can compose complex structures tree representations even more easily.
 */
trait TreePrinter[T] {

  def print(t: T): String = ???

  def contramap[U](f: U => T): TreePrinter[U] = ???

  protected def mkTree(t: T): TreePrinter.Tree

  private def toLines(subtree: TreePrinter.Tree): List[String] = subtree match {
    case TreePrinter.Tree(lines, Nil) => lines
    case TreePrinter.Tree(lines, nonEmptyChildren) =>
      val last = toLines(nonEmptyChildren.last)
      val prefixedLast = ("└── " + last.head) :: last.tail.map("    ".+)
      val prefixedInit = nonEmptyChildren.init.map { t =>
        val notTheLast = toLines(t)
        ("├── " + notTheLast.head) :: notTheLast.tail.map("│   ".+)
      }
      lines ::: prefixedInit.foldRight(prefixedLast)(_ ::: _)
  }
}

object TreePrinter {

  case class Tree(valueLines: List[String], children: List[Tree])

  def printTree[T : TreePrinter](t: T): String = ???

  // TODO: #1 - write TreePrinters for all primitives
  // TODO: #2 - Augment with typeclass constructors for stdlib constructs (collections, Option, Try, Either, Tuples, ...)

  implicit val treeTreePrinter: TreePrinter[Tree] = ???

  implicit val intsTreePrinter: TreePrinter[Int] = (i: Int) => Tree(String.valueOf(i) :: Nil, Nil)

  implicit val stringsTreePrinter: TreePrinter[String] = (s: String) => Tree(s.linesIterator.toList, Nil)

  implicit def tuple2TreePrinter[A : TreePrinter, B : TreePrinter]: TreePrinter[(A, B)] = {
    val tupleValueLines: List[String] = List(classOf[(_, _)].getSimpleName)
    val aTreePrinter: TreePrinter[A] = implicitly[TreePrinter[A]]
    val bTreePrinter: TreePrinter[B] = implicitly[TreePrinter[B]]

    new TreePrinter[(A, B)] {
      override protected def mkTree(t: (A, B)): Tree = Tree(
        tupleValueLines,
        List(
          aTreePrinter.mkTree(t._1),
          bTreePrinter.mkTree(t._2)))
    }
  }
}
