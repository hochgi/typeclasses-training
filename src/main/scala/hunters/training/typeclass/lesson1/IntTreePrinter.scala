package hunters.training.typeclass.lesson1

object IntTreePrinter {

  /**
   * Prints a tree in the same format of the terminal `tree` command.
   * Used characters:
   * ├, ─, └, │
   *
   * @param tree the tree to print
   * @return printed string
   */
  def print(tree: IntTree): String = {

    /**
     * DFS the tree, and append to the provided StringBuilder
     *
     * @param prefix prepend every line with prefix
     * @param headPrefixSuffix the current node is prepended with this as well
     * @param tailPrefixSuffix any child of the current node, has this appended to the prefix
     * @param sb the StringBuilder to append to
     * @param subtree the current tree node
     *
     * @return same StringBuilder for convenience chaining
     */
    def appendChildRec(prefix:           String,
                       headPrefixSuffix: String,
                       tailPrefixSuffix: String,
                       sb:               StringBuilder,
                       subtree:          IntTree): StringBuilder = {

      sb.append(prefix)
        .append(headPrefixSuffix)
        .append(String.valueOf(subtree.value))
        .append('\n')

      subtree match {
        case IntTree(_, Nil) => sb
        case IntTree(_, nonEmptyChildren) =>
          nonEmptyChildren.init.foreach { notTheLast =>
              appendChildRec(prefix + tailPrefixSuffix, "├── ", "│   ", sb, notTheLast)
          }
          appendChildRec(prefix + tailPrefixSuffix, "└── ", "    ", sb, nonEmptyChildren.last)
          sb
      }
    }

    appendChildRec("", "", "", new StringBuilder(), tree)
      .dropRight(1) // every line is appended with '\n', so finally drop the last one.
      .result()
  }
}
