package hunters.training.typeclass.lesson5

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

  describe("Lesson 5") {

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
                                                      |├── Brooklyn 99
                                                      |├── departments
                                                      |│   ├── 99th Precinct
                                                      |│   ├── teams
                                                      |│   │   ├── Police Reform
                                                      |│   │   ├── members
                                                      |│   │   │   ├── Terry Jeffords
                                                      |│   │   │   ├── 1000
                                                      |│   │   │   ├── Amy Santiago
                                                      |│   │   │   ├── 100
                                                      |│   │   │   ├── Jake Peralta
                                                      |│   │   │   ├── 99
                                                      |│   │   │   ├── Michael Hitchcock
                                                      |│   │   │   ├── 98
                                                      |│   │   │   ├── Norm Scully
                                                      |│   │   │   ├── 97
                                                      |│   │   │   ├── Charles Boyle
                                                      |│   │   │   ├── 96
                                                      |│   │   │   ├── Rosa Diaz
                                                      |│   │   │   └── 95
                                                      |│   │   ├── Police Administrative Aide
                                                      |│   │   └── members
                                                      |│   │       ├── Gina Linetti
                                                      |│   │       ├── 101
                                                      |│   │       ├── Ronald
                                                      |│   │       └── 102
                                                      |│   └── managers
                                                      |│       ├── Deputy Commissioner 1
                                                      |│       │   ├── Podolski
                                                      |│       │   └── 123
                                                      |│       ├── Deputy Commissioner 2
                                                      |│       │   ├── Aggerton
                                                      |│       │   └── 321
                                                      |│       ├── Deputy Commissioner 3
                                                      |│       │   ├── Grayson
                                                      |│       │   └── 312
                                                      |│       └── Deputy Commissioner 4
                                                      |│           ├── Raymond Holt
                                                      |│           └── 231
                                                      |└── chiefs
                                                      |    ├── Commissioner of the New York Police Department
                                                      |    │   ├── John Kelly
                                                      |    │   └── 1
                                                      |    └── Commissioner of the NYPD
                                                      |        ├── Madeline Wuntch
                                                      |        └── 2""".stripMargin
    }
  }
}
