package hunters.training.typeclass.lesson1

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class IntTreePrinterTests extends AnyFunSpec with Matchers {

  describe("IntTree printing") {
    it("should yield correct representation for a single node tree") {
      val s = IntTreePrinter.print(IntTree(42, Nil))
      s shouldEqual "42"
    }

    it("should yield correct representation for a simple tree") {
      val s = IntTreePrinter.print(IntTree(42, List(IntTree(24, Nil))))
      s shouldEqual """42
                      |└── 24""".stripMargin
    }

    it("should yield correct representation for a nested simple tree") {
      val s = IntTreePrinter.print(IntTree(1, List(IntTree(2, List(IntTree(3, Nil))))))
      s shouldEqual """1
                      |└── 2
                      |    └── 3""".stripMargin
    }

    it("should yield correct representation for a non-simple tree") {
      val s = IntTreePrinter.print(IntTree(1, List(IntTree(2, List(IntTree(3, Nil))), IntTree(4, Nil))))
      s shouldEqual """1
                      |├── 2
                      |│   └── 3
                      |└── 4""".stripMargin
    }

    it("should yield correct representation for nested non-simple tree") {
      val s = IntTreePrinter.print(IntTree(1, List(IntTree(2, List(IntTree(3, Nil), IntTree(4, Nil))), IntTree(5, Nil))))
      s shouldEqual """1
                      |├── 2
                      |│   ├── 3
                      |│   └── 4
                      |└── 5""".stripMargin
    }
  }
}
