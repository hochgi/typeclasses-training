package hunters.training.typeclass.lesson3

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TreePrinterTests extends AnyFunSpec with Matchers {

  // TODO: write more tests
  describe("Lesson 3 sanity") {
    it("should make a tree from a tuple") {
      TreePrinter.printTree("hello" -> (42 -> "world")) shouldEqual """.
                                                                      |├── hello
                                                                      |├── 42
                                                                      |└── world""".stripMargin
    }

    it("should make a tree from a tuple3") {
      TreePrinter.printTree(("hello", 42, "world")) shouldEqual """.
                                                                  |├── hello
                                                                  |├── 42
                                                                  |└── world""".stripMargin
    }

    it("should make a tree from a Map") {

      val m = Map(
        "hello" -> (1 -> "world"),
        "typeclasses" -> (2 -> "are\nawesome!"),
        "hunters" -> (4 -> "ever")
      )

      TreePrinter.printTree(m) shouldEqual """.
                                             |├── hello
                                             |│   ├── 1
                                             |│   └── world
                                             |├── typeclasses
                                             |│   ├── 2
                                             |│   └── are
                                             |│       awesome!
                                             |└── hunters
                                             |    ├── 4
                                             |    └── ever""".stripMargin
    }
  }
}
