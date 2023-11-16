package hunters.training.typeclass.lesson4

import magnolia1.{Magnolia, ReadOnlyCaseClass, SealedTrait}

import language.experimental.macros

trait TreePrinter[T] {

  def mkTrees(t: T): List[TreePrinter.Tree]

  def print(t: T): String = mkTrees(t) match {
    case Nil => ""
    case oneTree :: Nil => toLines(oneTree).mkString("\n")
    case nonEmptyForest => toLines(TreePrinter.Tree("." :: Nil, nonEmptyForest)).mkString("\n")
  }

  def contramap[U](f: U => T): TreePrinter[U] = (u: U) => mkTrees(f(u))

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

/**
 * generic derivation should have lower priority than concrete defined ones,
 * thus are placed in a trait for the companion object to mixin.
 */
trait GenericTreePrinter {
  type Typeclass[T] = TreePrinter[T]

  def join[T](ctx: ReadOnlyCaseClass[TreePrinter, T]): TreePrinter[T] = ???

  def split[T](ctx: SealedTrait[TreePrinter, T]): TreePrinter[T] = ???

  implicit def gen[T]: TreePrinter[T] = macro Magnolia.gen[T]
}

object TreePrinter extends GenericTreePrinter {


  trait TreeValuePrinter[T] {
    def toLines(t: T): List[String]
  }

  object TreeValuePrinter {

    implicit val intsTreePrinter: TreeValuePrinter[Int] = (i: Int) => String.valueOf(i) :: Nil

    implicit val stringsTreePrinter: TreeValuePrinter[String] = (s: String) => s.linesIterator.toList
  }

  case class Tree(valueLines: List[String], children: List[Tree])

  def printTree[T : TreePrinter](t: T): String = implicitly[TreePrinter[T]].print(t)

  implicit val treeTreePrinter: TreePrinter[Tree] = _ :: Nil

  implicit def singleNodeTree[T: TreeValuePrinter]: TreePrinter[T] = treeTreePrinter.contramap { (t: T) =>
    Tree(implicitly[TreeValuePrinter[T]].toLines(t), Nil)
  }

  implicit def mapTreePrinter[K: TreeValuePrinter, V: TreePrinter]: TreePrinter[Map[K, V]] = {
    val kTreeValuePrinter = implicitly[TreeValuePrinter[K]]
    val vTreePrinter = implicitly[TreePrinter[V]]

    (m: Map[K, V]) => m.view.map { case (k, v) =>
      Tree(kTreeValuePrinter.toLines(k), vTreePrinter.mkTrees(v))
    }.toList
  }

  /**
   * List is a valid ADT, try to not implement this, and check the results.
   * Then, since implicit from auto derivation has lower priority (due to implicit scope precedence),
   * provide a manual implementation here, and compare results.
   */
  // implicit def listTreePrinter[T: TreePrinter]: TreePrinter[List[T]] = ???
}
