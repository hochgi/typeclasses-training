package hunters.training.typeclass.lesson1

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class StringTreePrinterTests extends AnyFunSpec with Matchers {

  describe("StringTree printing") {
    describe("should preserve StringTree simple tests") {
      it("with yield correct representation for a single node tree") {
        val s = StringTreePrinter.print(StringTree("42", Nil))
        s shouldEqual "42"
      }

      it("with yield correct representation for a simple tree") {
        val s = StringTreePrinter.print(StringTree("42", List(StringTree("24", Nil))))
        s shouldEqual
          """42
            |└── 24""".stripMargin
      }

      it("with yield correct representation for a nested simple tree") {
        val s = StringTreePrinter.print(StringTree("1", List(StringTree("2", List(StringTree("3", Nil))))))
        s shouldEqual
          """1
            |└── 2
            |    └── 3""".stripMargin
      }

      it("with yield correct representation for a non-simple tree") {
        val s = StringTreePrinter.print(StringTree("1", List(StringTree("2", List(StringTree("3", Nil))), StringTree("4", Nil))))
        s shouldEqual
          """1
            |├── 2
            |│   └── 3
            |└── 4""".stripMargin
      }

      it("with yield correct representation for nested non-simple tree") {
        val s = StringTreePrinter.print(StringTree("1", List(StringTree("2", List(StringTree("3", Nil), StringTree("4", Nil))), StringTree("5", Nil))))
        s shouldEqual
          """1
            |├── 2
            |│   ├── 3
            |│   └── 4
            |└── 5""".stripMargin
      }
    }
    describe("should also be able to render multi-line strings") {
      it("with yield correct representation for a single node tree") {
        val s = StringTreePrinter.print(StringTree("4\n2", Nil))
        s shouldEqual "4\n2"
      }

      it("with yield correct representation for a simple tree") {
        val s = StringTreePrinter.print(StringTree("4\n2", List(StringTree("2\n4", Nil))))
        s shouldEqual
          """4
            |2
            |└── 2
            |    4""".stripMargin
      }

      it("with yield correct representation for a nested simple tree") {
        val s = StringTreePrinter.print(StringTree("1\n1", List(StringTree("2\n2", List(StringTree("3\n3", Nil))))))
        s shouldEqual
          """1
            |1
            |└── 2
            |    2
            |    └── 3
            |        3""".stripMargin
      }

      it("with yield correct representation for a non-simple tree") {
        val s = StringTreePrinter.print(StringTree("1\n1", List(StringTree("2\n2", List(StringTree("3\n3", Nil))), StringTree("4\n4", Nil))))
        s shouldEqual
          """1
            |1
            |├── 2
            |│   2
            |│   └── 3
            |│       3
            |└── 4
            |    4""".stripMargin
      }

      it("with yield correct representation for nested non-simple tree") {
        val s = StringTreePrinter.print(StringTree("1\n1\n1", List(StringTree("2\n2\n2", List(StringTree("3\n3\n3", Nil), StringTree("4\n4\n4", Nil))), StringTree("5\n5\n5", Nil))))
        s shouldEqual
          """1
            |1
            |1
            |├── 2
            |│   2
            |│   2
            |│   ├── 3
            |│   │   3
            |│   │   3
            |│   └── 4
            |│       4
            |│       4
            |└── 5
            |    5
            |    5""".stripMargin
      }
    }
  }
}
