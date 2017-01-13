package org.eoin

import org.eoin.Chapter10._
import org.eoin.Chapter10.exercise9.{Part, Stub, WC}
import org.junit.Test
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/**
  * Created by eoin.parker on 11/10/16.
  */
class Chapter10TestSuite extends JUnitSuite with GeneratorDrivenPropertyChecks {

  def associativeLaw[A] (m:Monoid[A]) (implicit arbA: Arbitrary[A]) = {
    forAll (minSuccessful(1000)) { (a1:A,a2:A,a3:A) =>
         m.op ( m.op(a1,a2), a3)  should be ( m.op(m.op(a1,a2), a3))
    }
  }

  def zeroElement[A] (m:Monoid[A]) (implicit arbA: Arbitrary[A]) = {
    forAll (minSuccessful(1000)) { (a:A) =>
      //println(a)
      m.op (a, m.zero)  should be (a)
      m.op (m.zero, a)  should be (a)

    }
  }

  @Test def exercise1BasicMonoids : Unit = {

        associativeLaw(exercise1.stringMonoid)
    associativeLaw(exercise1.listMonoid[Int])
   zeroElement(exercise1.stringMonoid)
   zeroElement(exercise1.listMonoid[Int])


    associativeLaw(exercise1.intAddition)
    associativeLaw(exercise1.intMultiplication)
    associativeLaw(exercise1.booleanOr)
    associativeLaw(exercise1.booleanAnd)

   zeroElement(exercise1.intAddition)
    zeroElement(exercise1.intMultiplication)
    zeroElement(exercise1.booleanOr)
    zeroElement(exercise1.booleanAnd)

     }

  @Test def exercise2OptionMonoid : Unit = {
    associativeLaw(exercise2.optionMonoid[String])
    zeroElement(exercise2.optionMonoid[String])

  }

  @Test def exercise3EndoFunctionMonoid : Unit = {
    associativeLaw(exercise3.EndoMonoid[String])
    zeroElement(exercise3.EndoMonoid[String])

  }

  @Test def exercise5WordsMonoid : Unit = {
    associativeLaw(exercise5.wordsMonoid(" "))
    zeroElement(exercise5.wordsMonoid(" "))

  }

  @Test def exercise6Concatenate : Unit = {
    forAll {(ls:List[Int]) =>
      exercise6.concatenate(ls, exercise1.intAddition) should equal(ls.sum)
    }
  }

  @Test def exercise9WCMonoid : Unit = {

    val stubGen = for {
      s1 <- Gen.alphaStr
    } yield {Stub(s1)}

    val partGen = for {
      s1 <- Gen.alphaStr
      s2 <- Gen.alphaStr
      i <- Gen.posNum[Int]
    } yield { Part(s1,i,s2) }

    val wcArb:Arbitrary[WC] = Arbitrary { Gen.oneOf(stubGen,partGen) }

    associativeLaw(exercise9.wcMonoid)(wcArb)
    zeroElement(exercise9.wcMonoid)(wcArb)
  }

    @Test def exercise10WordCountWithMonoid : Unit = {

      forAll (minSuccessful(1000)) { (s:String) =>
        val wordcounterRegex = """\b\w+\b""".r
        val regexCount = wordcounterRegex.findAllIn(s).count(_ => true)
        exercise10.countWords(s) should be (regexCount)
      }

    }

    @Test def exercise11EfficientFoldMapV : Unit = {
      forAll (minSuccessful(1000)) { (is:IndexedSeq[Int]) =>
        val monoidSumOfSquares = exercise11.foldMapV(is, exercise1.intAddition) { _^2 }
        val referenceSumOfSquares = (is map { _^2}).sum
        //println (is.size, referenceSumOfSquares, is)
        monoidSumOfSquares should be( referenceSumOfSquares)
      }
    }

    @Test def exercise12IsOrdered : Unit = {

      associativeLaw(exercise12.compareMonoid)
      zeroElement(exercise12.compareMonoid)

      val orderedGen = Gen.containerOf[IndexedSeq, Int] (Arbitrary.arbitrary[Int])

      forAll (minSuccessful(1000)) { (is:IndexedSeq[Int], makeOrdered:Boolean) =>
        val input = if (makeOrdered) is.sorted else is
        val isSortedReferenceResult = (input == input.sorted)
        val isSortedMonoidResult = exercise12.isOrdered(input)

        isSortedMonoidResult should be (isSortedReferenceResult)
    }
    }


      @Test def fails : Unit = {

        val isSortedMonoidResult = exercise12.isOrdered(Vector(0, -1, 0))
        println(isSortedMonoidResult)
      }


}
