package hunters.training.typeclass.lesson3

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TreePrinterTests extends AnyFunSpec with Matchers {

  // TODO: write more tests
  describe("Lesson 3 sanity") {
    it("should make a tree from a tuple") {
      TreePrinter.printTree("hello" -> (42 -> "world")) shouldEqual """Tuple2
                                                                      |├── hello
                                                                      |└── Tuple2
                                                                      |    ├── 42
                                                                      |    └── world""".stripMargin
    }
  }
}
