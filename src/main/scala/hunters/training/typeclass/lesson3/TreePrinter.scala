package hunters.training.typeclass.lesson3


/**
 * To make [[TreePrinter]]s even more composable and simple,
 * instead of forcing library users to know (and use) [[TreePrinter.Tree]],
 * we can add composability to our typeclass, and only keeping the tree internally.
 * This way, we can compose complex structures tree representations even more easily.
 */
trait TreePrinter[T] {

  def print(t: T): String = toLines(mkTree(t)).mkString("\n")

  def contramap[U](f: U => T): TreePrinter[U] = (u: U) => mkTree(f(u))

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

trait TreeValuePrinter[T] {
  def toLines(t: T): List[String]
}

object TreePrinter {

  case class Tree(valueLines: List[String], children: List[Tree])

  def printTree[T : TreePrinter](t: T): String = implicitly[TreePrinter[T]].print(t)

  // TODO: #1 - write TreePrinters for all primitives
  // TODO: #2 - Augment with typeclass constructors for stdlib constructs (collections, Option, Try, Either, Tuples, ...)

  implicit val treeTreePrinter: TreePrinter[Tree] = identity[Tree]

  implicit val intsTreePrinter: TreePrinter[Int] = (i: Int) => Tree(String.valueOf(i) :: Nil, Nil)

  implicit val stringsTreePrinter: TreePrinter[String] = (s: String) => Tree(s.linesIterator.toList, Nil)

  implicit def tuple2TreePrinter[A: TreePrinter, B: TreePrinter]: TreePrinter[(A, B)] = {
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

  /**
   * Problem: we would want to make a typeclass constructor for Maps.
   * But maps have 2 type parameters, and it only makes sense to require the values to have a TreePrinter typeclass.
   * The keys should be represented as Tree values. I.E: a list of strings.
   * So How can we implement the following?
   *
   * Solution: We need another typeclass for making a Value, i.e. [[List]] os [[String]]s
   *
   * Problem: For every "primitive", like [[Int]] or [[String]] we now need 2 typeclasses.
   * 1 for Tree, the other for TreeValue.
   * And logic is duplicated.
   */
  implicit def mapTreePrinter[K: TreeValuePrinter, V: TreePrinter]: TreePrinter[Map[K, V]] = ???
}
