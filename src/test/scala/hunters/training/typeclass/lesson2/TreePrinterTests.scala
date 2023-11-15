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

      it("with yield correct representation for an Either[Int, String] simple tree") {
        val s = TreePrinter.print[Either[Int, String]](Tree(Left(42), List(Tree(Right("24"), Nil))))
        s shouldEqual
          """42
            |└── 24""".stripMargin
      }
    }
    describe("should also be able to render multi-line strings") {
      it("with yield correct representation for a single node tree") {
        val s = TreePrinter.print(Tree(List(4, 2), Nil))
        s shouldEqual "4\n2"
      }

      it("with yield correct representation for a simple tree") {
        val s = TreePrinter.print[List[Either[String, Int]]](Tree(List(Left("4"), Right(2)), List(Tree(List(Right(2),Left("4")), Nil))))
        s shouldEqual
          """4
            |2
            |└── 2
            |    4""".stripMargin
      }

      it("with yield correct representation for nested non-simple tree") {
        val s = TreePrinter.print[List[String]](
          Tree(
            List("1\n1\n1"),
            List(
              Tree(
                List("2", "2\n2"),
                List(
                  Tree(
                    List("3", "3", "3"),
                    Nil),
                  Tree(
                    List("4\n4", "4"),
                    Nil)
                )
              ),
              Tree(
                List("5\n5\n5"),
                Nil)
            )
          )
        )

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
    describe("recursive tree prints should be possible") {
      it("with recursive simple tree"){
        val s = TreePrinter.print(Tree(
          value = Tree(42, Nil),
          children = Nil))
        s shouldEqual "42"
      }

      it("with recursive complex tree"){

        val value: Tree[Either[Int, String]] = Tree(Left(42), List(Tree(Right("24"), Nil)))

        val s = TreePrinter.print(Tree(
          value,
          children = List(Tree(value, Nil))))

        s shouldEqual """42
                        |└── 24
                        |└── 42
                        |    └── 24""".stripMargin
      }
    }
  }
}
