package pt.rmartins.battleships.backend.services

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UtilsTest extends AnyWordSpec with Matchers {

  private val A = "A"
  private val B = "B"
  private val C = "C"
  private val D = "D"
  private val E = "E"

  "combinationGroups" should {

    "return correct combinations of {} in two groups [1]" in {
      Utils.combinationGroups(List(), List(1)) should be(
        Set()
      )
    }

    "return correct combinations of {A} in two groups [0]" in {
      Utils.combinationGroups(List(A), List(0)) should be(
        Set(Set(Set()))
      )
    }

    "return correct combinations of {A} in two groups [2]" in {
      Utils.combinationGroups(List(A), List(2)) should be(
        Set()
      )
    }

    "return correct combinations of {A,B} in two groups [1]" in {
      Utils.combinationGroups(List(A, B), List(1)) should be(
        Set(
          Set(Set(A)),
          Set(Set(B))
        )
      )
    }

    "return correct combinations of {A,B} in two groups [1,1]" in {
      Utils.combinationGroups(List(A, B), List(1, 1)) should be(
        Set(
          Set(Set(A), Set(B))
        )
      )
    }

    "return correct combinations of {A,B,C} in two groups [1,1]" in {
      Utils.combinationGroups(List(A, B, C), List(1, 1)) should be(
        Set(
          Set(Set(A), Set(B)),
          Set(Set(A), Set(C)),
          Set(Set(B), Set(C))
        )
      )
    }

    "return correct combinations of [A,B,C} in two groups (2,1)" in {
      Utils.combinationGroups(List(A, B, C), List(2, 1)) should be(
        Set(
          Set(Set(A, B), Set(C)),
          Set(Set(A, C), Set(B)),
          Set(Set(B, C), Set(A))
        )
      )
    }

    "return correct combinations of {A,B,C,D} in two groups (2,2)" in {
      Utils.combinationGroups(List(A, B, C, D), List(2, 2)) should be(
        Set(
          Set(Set(A, B), Set(C, D)),
          Set(Set(A, C), Set(B, D)),
          Set(Set(A, D), Set(B, C))
        )
      )
    }

    "return correct combinations of {A,B,C,D,E} in two groups (3,2)" in {
      Utils.combinationGroups(List(A, B, C, D, E), List(3, 2)) should be(
        Set(
          Set(Set(A, B), Set(C, D, E)),
          Set(Set(A, C), Set(B, D, E)),
          Set(Set(A, D), Set(B, C, E)),
          Set(Set(A, E), Set(B, C, D)),
          Set(Set(B, C), Set(A, D, E)),
          Set(Set(B, D), Set(A, C, E)),
          Set(Set(B, E), Set(A, C, D)),
          Set(Set(C, D), Set(A, B, E)),
          Set(Set(C, E), Set(A, B, D)),
          Set(Set(D, E), Set(A, B, C))
        )
      )
    }

  }

}
