package hunters.training.typeclass.lesson4

import hunters.training.typeclass.lesson4.TreePrinterTests._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

object TreePrinterTests {
  case class Employee(name: String, number: Int)
  case class Team(name: String, members: List[Employee])
  case class Department(name: String, teams: List[Team], managers: Map[String, Employee])
  case class Organization(name: String, departments: List[Department], chiefs: Map[String, Employee])
}
class TreePrinterTests extends AnyFunSpec with Matchers {

  describe("Lesson 4") {

    it("should make a tree from a Map") {

      val m = Map(
        "hello" -> (1 -> "world"),
        "typeclasses" -> (2 -> "are\nawesome!"),
        "hunters" -> (4 -> "ever")
      )

      /*  In our new code, we removed the special treatment for tuples,
       *  as those are just another form of case classes,
       *  and we now deal with them generically.
       *
       * Our generic implementation uses field names as value lines in the tree,
       * so we needed to "fix" the test to show the output properly.
       */
      TreePrinter.printTree(m) shouldEqual """.
                                             |├── hello
                                             |│   ├── _1
                                             |│   │   └── 1
                                             |│   └── _2
                                             |│       └── world
                                             |├── typeclasses
                                             |│   ├── _1
                                             |│   │   └── 2
                                             |│   └── _2
                                             |│       └── are
                                             |│           awesome!
                                             |└── hunters
                                             |    ├── _1
                                             |    │   └── 4
                                             |    └── _2
                                             |        └── ever""".stripMargin
    }


    it("should make a tree from a derived magnolia typeclass") {

      val brooklyn99 = Organization(
        name = "Brooklyn 99",
        departments = List(
          Department(
            "99th Precinct",
            teams = List(
              Team(
                name = "Police Reform",
                members = List(
                  Employee("Terry Jeffords", 1000),
                  Employee("Amy Santiago", 100),
                  Employee("Jake Peralta", 99),
                  Employee("Michael Hitchcock", 98),
                  Employee("Norm Scully", 97),
                  Employee("Charles Boyle", 96),
                  Employee("Rosa Diaz", 95)
                )
              ),
              Team(
                name = "Police Administrative Aide",
                members = List(
                  Employee("Gina Linetti", 101),
                  Employee("Ronald", 102)
                )
              )
            ),
            managers = Map(
              "Deputy Commissioner 1" -> Employee("Podolski", 123),
              "Deputy Commissioner 2" -> Employee("Aggerton", 321),
              "Deputy Commissioner 3" -> Employee("Grayson", 312),
              "Deputy Commissioner 4" -> Employee("Raymond Holt", 231)
            )
          )
        ),
        chiefs = Map(
          "Commissioner of the New York Police Department" -> Employee("John Kelly", 1),
          "Commissioner of the NYPD" -> Employee("Madeline Wuntch", 2)
        )
      )

      TreePrinter.printTree(brooklyn99) shouldEqual """.
                                                      |├── name
                                                      |│   └── Brooklyn 99
                                                      |├── departments
                                                      |│   ├── name
                                                      |│   │   └── 99th Precinct
                                                      |│   ├── teams
                                                      |│   │   ├── name
                                                      |│   │   │   └── Police Reform
                                                      |│   │   ├── members
                                                      |│   │   │   ├── name
                                                      |│   │   │   │   └── Terry Jeffords
                                                      |│   │   │   ├── number
                                                      |│   │   │   │   └── 1000
                                                      |│   │   │   ├── name
                                                      |│   │   │   │   └── Amy Santiago
                                                      |│   │   │   ├── number
                                                      |│   │   │   │   └── 100
                                                      |│   │   │   ├── name
                                                      |│   │   │   │   └── Jake Peralta
                                                      |│   │   │   ├── number
                                                      |│   │   │   │   └── 99
                                                      |│   │   │   ├── name
                                                      |│   │   │   │   └── Michael Hitchcock
                                                      |│   │   │   ├── number
                                                      |│   │   │   │   └── 98
                                                      |│   │   │   ├── name
                                                      |│   │   │   │   └── Norm Scully
                                                      |│   │   │   ├── number
                                                      |│   │   │   │   └── 97
                                                      |│   │   │   ├── name
                                                      |│   │   │   │   └── Charles Boyle
                                                      |│   │   │   ├── number
                                                      |│   │   │   │   └── 96
                                                      |│   │   │   ├── name
                                                      |│   │   │   │   └── Rosa Diaz
                                                      |│   │   │   └── number
                                                      |│   │   │       └── 95
                                                      |│   │   ├── name
                                                      |│   │   │   └── Police Administrative Aide
                                                      |│   │   └── members
                                                      |│   │       ├── name
                                                      |│   │       │   └── Gina Linetti
                                                      |│   │       ├── number
                                                      |│   │       │   └── 101
                                                      |│   │       ├── name
                                                      |│   │       │   └── Ronald
                                                      |│   │       └── number
                                                      |│   │           └── 102
                                                      |│   └── managers
                                                      |│       ├── Deputy Commissioner 1
                                                      |│       │   ├── name
                                                      |│       │   │   └── Podolski
                                                      |│       │   └── number
                                                      |│       │       └── 123
                                                      |│       ├── Deputy Commissioner 2
                                                      |│       │   ├── name
                                                      |│       │   │   └── Aggerton
                                                      |│       │   └── number
                                                      |│       │       └── 321
                                                      |│       ├── Deputy Commissioner 3
                                                      |│       │   ├── name
                                                      |│       │   │   └── Grayson
                                                      |│       │   └── number
                                                      |│       │       └── 312
                                                      |│       └── Deputy Commissioner 4
                                                      |│           ├── name
                                                      |│           │   └── Raymond Holt
                                                      |│           └── number
                                                      |│               └── 231
                                                      |└── chiefs
                                                      |    ├── Commissioner of the New York Police Department
                                                      |    │   ├── name
                                                      |    │   │   └── John Kelly
                                                      |    │   └── number
                                                      |    │       └── 1
                                                      |    └── Commissioner of the NYPD
                                                      |        ├── name
                                                      |        │   └── Madeline Wuntch
                                                      |        └── number
                                                      |            └── 2""".stripMargin
    }
  }
}
