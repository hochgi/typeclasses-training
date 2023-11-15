package hunters.training.typeclass.lesson2

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TreePrinterTests extends AnyFunSpec with Matchers {
  describe("Tree printing") {
    describe("should preserve some of the previous tests") {
      it("with yield correct representation for a single Int node tree") {
        val s = TreePrinter.print(Tree(42, Nil))
        s shouldEqual "42"
      }

      it("with yield correct representation for a single String node tree") {
        val s = TreePrinter.print(Tree("42", Nil))
        s shouldEqual "42"
      }
    }
  }
}
