import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.state.RNG
import fpinscala.testing._

/*
Ex. 8.1

Some properties (expressed as functions) to test an implementation of
sum: List[Int] => Int

  1. xs: List[Int] => sum(xs) == sum(xs.reverse)

  2. n: Int => x: Int => sum(List.fill(n)(x)) == n * x

  3. sum(List.empty) == 0

  4. x: Int => sum(List(x)) == x

Ex. 8.2

Properties that specify an implementation of maximum: List[Int] => Int

  1. x: Int => maximum(List(x)) == x

  2. n: Int => x: Int => maximum(List.fill(n)(x)) == x

  3. xs: List[Int] => maximum(xs) == maximum(xs.reverse)
*/

class GenSpec extends Specification with ScalaCheck {
  val simpleRng = RNG.Simple(1)
  val pPass = new Prop { def check = Right(1) }
  val leftFail = Left("Fail" -> 1)
  val pFail = new Prop { def check = leftFail }

  def genRun[A](g: Gen[A]): A = g.sample.run(simpleRng)._1

  "Prop#&&" should {
    "result in true if all individual Props result in true" in {
      val p = new Prop { def check = Right(1) }
      (pPass && p).check mustEqual Right(2)
    }

    "result in false if some individual Props result in false" in {
      (pPass && pFail).check mustEqual leftFail
    }
  }

  "Gen.choose" should {
    "return a number within the given range" in {
      val start = 0
      val stopExclusive = 10
      val num = genRun(Gen.choose(start, stopExclusive))

      num must beBetween(start, stopExclusive).excludingEnd
    }
  }

  "Gen.unit" should {
    "always result in its input value" in {
      prop { inputVal: Int =>
        val num = genRun(Gen.unit(inputVal))
        num mustEqual inputVal
      }
    }
  }

  "Gen.boolean" should {
    "always result in true or false" in {
      Seq(true, false) must contain(genRun(Gen.boolean))
    }
  }

  "Gen.listOfN" should {
    "result in a list of the given size" in {
      val n = 5
      val g = Gen.unit(1)
      val listGen = Gen.listOfN(n, g)

      genRun(listGen).length mustEqual n
    }
  }
}

