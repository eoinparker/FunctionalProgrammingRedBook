package org.eoin

import org.eoin.Chapter2.{Exercise1, Exercise2}
import org.junit.Test
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.collection.BitSet
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.HashSet

/**
  * Created by eoin.parker on 11/10/16.
  */
class Chapter2TestSuite extends JUnitSuite with GeneratorDrivenPropertyChecks {

  @Test def test1Exercise1Fibonacci : Unit = {

    forAll (minSuccessful(20)) {   ( n: Int ) =>
      whenever(n > 0 && n < Integer.MAX_VALUE) {
        val nth = Exercise1.fib(n)
        val `n + 1'th` = Exercise1.fib(n + 1)
        val `n + 2'th` = Exercise1.fib(n + 2)

        `n + 2'th` should equal(nth + `n + 1'th`)
      }
    }
  }

  @Test def test1Exercise2Sorting : Unit = {

    val genStringArray = Gen.containerOf[Array,String] (Arbitrary.arbitrary[String])  //(Gen.alphaStr )
    val genStringCompFn = Gen.oneOf[(String,String) => Boolean](
        (s1:String, s2:String) => s1.compareTo(s2) > 0 ,
        (s1:String, s2:String) => s1.hashCode > s2.hashCode,
        (s1:String, s2:String) => s1.length > s2.length
      )

    forAll  ((genStringArray, "array to sort"), (genStringCompFn, "compare fn"), minSuccessful(1000)) {
      ( as: Array[String], compareFn: (String,String) => Boolean )   =>
        val ourFnSays = Exercise2.isSorted(as, compareFn)
        val referenceFnSays = as.sortWith(compareFn).sameElements(as)
        ourFnSays shouldEqual referenceFnSays
      }
  }

//  @Test def test2Exercise2:Unit = {
//    val as = Array(" ", "")
//    val compFn1 = (s1:String, s2:String) => s1.compareTo(s2) > 0
//    val compFn2 = (s1:String, s2:String) => s1.hashCode > s2.hashCode
//    val compFn3 = (s1:String, s2:String) => s1.length > s2.length
//
//    logger.info(exercise2.isSorted(as, compFn1))
//    logger.info(exercise2.isSorted(as, compFn2))
//    logger.info(exercise2.isSorted(as, compFn3))
//
//    logger.info(as.sortWith(compFn1).sameElements(as))
//    logger.info(as.sortWith(compFn2).sameElements(as))
//    logger.info(as.sortWith(compFn3).sameElements(as))
//
////    logger.info(as.sorted.sameElements(as))
////    logger.info(as.sorted.sameElements(as))
////    logger.info(as.sorted.sameElements(as))
//  }

  @Test def test1Exercise4Currying : Unit = {

    //val mapMaker = Gen.containerOf(Map, (String, (Int, Boolean)))

//    forAll (minSuccessful(100)) (mapMaker) { (s:String,(i:Int, b:Boolean)) =>
//      exercise4.curry ()
//
//    }


  }

  @Test def testCBF: Unit = {

    import scala.language.higherKinds


    def combineValues[U, T[_]](pairs: Seq[(U, U)])(
      implicit cbf: CanBuildFrom[T[U], U, T[U]]): Seq[(U, T[U])] = {

      val prepped = pairs.groupBy { _._1 } .mapValues { _.map { _._2 } }
      val processed = prepped.map { case (k, vSeq) =>
        val builder = cbf()
        vSeq foreach {builder += _ } // impure ?
        (k, builder.result())
      }
      processed.toList
    }

    // collection equality says that the CBF-created output List[String]
    // and the CBF-created output Vector[AnyRef] should come out the same
    forAll { pairs: Seq[(String, String)] =>
      combineValues[String, List](pairs) should equal (combineValues[AnyRef, Vector](pairs))
    }

    type MyBitSet[_] = BitSet  // hack.  cbf needs a type param.  put in a dummy one for the test
    // need positive ints only for testing against BitSet
    val gP = Gen.posNum[Int]
    val genPosTuple:Gen[(Int,Int)] = for { a <- gP ; b <- gP } yield (a,b)
    val genSeqPosTuples = Gen.containerOf[Seq, (Int,Int)](genPosTuple)

    forAll (genSeqPosTuples) { pairs: Seq[(Int, Int)] =>
      whenever (pairs.forall { p => p._1 > 0 && p._2 > 0} ) {
        combineValues[AnyVal, HashSet](pairs) should equal(combineValues[Int, MyBitSet](pairs))
      }
    }

  }



}
