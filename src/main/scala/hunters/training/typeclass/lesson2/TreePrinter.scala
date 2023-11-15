package hunters.training.typeclass.lesson2

object TreePrinter {

  /**
   * Prints a tree in the same format of the terminal `tree` command.
   * Used characters:
   * ├, ─, └, │
   *
   * @param tree the tree to print
   * @return printed string
   */
  def print[T: Printer](tree: Tree[T]): String = ???
}
