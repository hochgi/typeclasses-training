package hunters.training.typeclass.lesson3

import scala.collection.immutable


/**
 * To make [[TreePrinter]]s even more composable and simple,
 * instead of forcing library users to know (and use) [[TreePrinter.Tree]],
 * we can add composability to our typeclass, and only keeping the tree internally.
 * This way, we can compose complex structures tree representations even more easily.
 */
trait TreePrinter[T] {

  def print(t: T): String = ???

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
object TreeValuePrinter {

  implicit val intsTreePrinter: TreeValuePrinter[Int] = (i: Int) => String.valueOf(i) :: Nil

  implicit val stringsTreePrinter: TreeValuePrinter[String] = (s: String) => s.linesIterator.toList
}

object TreePrinter {

  case class Tree(valueLines: List[String], children: List[Tree])

  def printTree[T : TreePrinter](t: T): String = implicitly[TreePrinter[T]].print(t)

  // TODO: #1 - write TreePrinters for all primitives
  // TODO: #2 - Augment with typeclass constructors for stdlib constructs (collections, Option, Try, Either, Tuples, ...)

  implicit val treeTreePrinter: TreePrinter[Tree] = ???

  implicit def singleNodeTree[T: TreeValuePrinter]: TreePrinter[T] = ???

  implicit def tuple2TreePrinter[A: TreePrinter, B: TreePrinter]: TreePrinter[(A, B)] = ???

  implicit def tuple3TreePrinter[A: TreePrinter, B: TreePrinter, C: TreePrinter]: TreePrinter[(A, B, C)] = ???

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
   *
   * Solution: We'll implement TreePrinter constructor from TreeValuePrinter typeclass just once
   *
   * Problem: So far our recursive derivation was 1:1, i.e: we could not easily use the fact that trees has multiple children,
   * since we have no way to refer to multiple children that are not rooted in a single key.
   * In the tuple example we worked around it by adding a synthetic key that is the type name ("Tuple2").
   * Going forward, we want to be able to render un-rooted trees. or "forest" if you will, which will help use implement
   * map derivation functionality properly.
   */
  implicit def mapTreePrinter[K: TreeValuePrinter, V: TreePrinter]: TreePrinter[Map[K, V]] = {
    val kTreeValuePrinter = implicitly[TreeValuePrinter[K]]
    val vTreePrinter = implicitly[TreePrinter[V]]

    new TreePrinter[Map[K, V]] {
      override protected def mkTree(m: Map[K, V]): Tree = {
        val subtrees: immutable.Iterable[Tree] = m.map { case (k, v) =>
          Tree(kTreeValuePrinter.toLines(k), vTreePrinter.mkTree(v) :: Nil)
        }

        ???
      }
    }
  }
}
