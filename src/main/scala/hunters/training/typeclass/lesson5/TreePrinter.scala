package hunters.training.typeclass.lesson5

import magnolia1.{Magnolia, ReadOnlyCaseClass, SealedTrait}

import language.experimental.macros

trait TreePrinter[T] {

  def mkValueOrTrees(t: T): Either[List[String], List[TreePrinter.Tree]]

  def print(t: T): String = mkValueOrTrees(t) match {
    case Left(valueLines) => valueLines.mkString("\n")
    case Right(forest) => forestToLines(forest).mkString("\n")
  }

  def forestToLines(forest: List[TreePrinter.Tree]): List[String] = forest match {
    case Nil => Nil
    case oneTree :: Nil => toLines(oneTree)
    case nonEmptyForest => toLines(TreePrinter.Tree("." :: Nil, nonEmptyForest))
  }

  def contramap[U](f: U => T): TreePrinter[U] = (u: U) => mkValueOrTrees(f(u))

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

  def join[T](ctx: ReadOnlyCaseClass[TreePrinter, T]): TreePrinter[T] = (value: T) => Right {
    ctx.parameters.view.map { param =>
      param.typeclass.mkValueOrTrees(param.dereference(value)) match {
        case Left(valueLines) => TreePrinter.Tree(valueLines, Nil)
        case Right(manyTrees) => TreePrinter.Tree(param.label :: Nil, manyTrees)
      }
    }.toList
  }

  def split[T](ctx: SealedTrait[TreePrinter, T]): TreePrinter[T] = (value: T) => {
    ctx.split(value) { sub =>
      sub.typeclass.mkValueOrTrees(sub.cast(value))
    }
  }

  implicit def gen[T]: TreePrinter[T] = macro Magnolia.gen[T]
}

object TreePrinter extends GenericTreePrinter {

  case class Tree(valueLines: List[String], children: List[Tree])

  def printTree[T: TreePrinter](t: T): String = implicitly[TreePrinter[T]].print(t)

  implicit val intsTreePrinter: TreePrinter[Int] = (i: Int) => Left(String.valueOf(i) :: Nil)

  implicit val stringsTreePrinter: TreePrinter[String] = (s: String) => Left(s.linesIterator.toList)

  implicit val identityTreePrinter: TreePrinter[Tree] = t => Right(t :: Nil)

  implicit def mapTreePrinter[K: TreePrinter, V: TreePrinter]: TreePrinter[Map[K, V]] = {
    val kTreePrinter = implicitly[TreePrinter[K]]
    val vTreePrinter = implicitly[TreePrinter[V]]

    (m: Map[K, V]) => Right(m.view.map { case (k, v) =>
      val keyValueLines = kTreePrinter.mkValueOrTrees(k) match {
        case Left(valueLines) => valueLines
        case Right(Tree(valueLines, Nil) :: Nil) => valueLines
        case Right(nonFlatSingleTree) => kTreePrinter.forestToLines(nonFlatSingleTree)
      }
      Tree(keyValueLines, vTreePrinter.mkValueOrTrees(v).fold(Tree(_, Nil) :: Nil, identity))
    }.toList)
  }

  implicit def listTreePrinter[T: TreePrinter]: TreePrinter[List[T]] = {
    val treePrinter = implicitly[TreePrinter[T]]
    (ts: List[T]) => {
      val (values, trees) = ts.partitionMap(treePrinter.mkValueOrTrees)
      Right(values.map(Tree(_, Nil)) ::: trees.flatten)
    }
  }
}
