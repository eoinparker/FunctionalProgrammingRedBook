package org.eoin

import org.junit.Test
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/**
  * Created by eoin.parker on 11/10/16.
  */
class Chapter2TestSuite extends JUnitSuite with GeneratorDrivenPropertyChecks {

  @Test def testFib : Unit = {

    forAll (minSuccessful(20)) {   ( n: Int ) =>
      whenever(n > 0 && n < Integer.MAX_VALUE) {
        val nth = exercise1.fib(n)
        val `n + 1'th` = exercise1.fib(n + 1)
        val `n + 2'th` = exercise1.fib(n + 2)

        `n + 2'th` should equal(nth + `n + 1'th`)
      }
    }
  }

  @Test def testIsSorted : Unit = {

    val getStringArray = Gen.containerOf[Array,String] (Arbitrary.arbitrary[String])  //(Gen.alphaStr )
    val stringCompare = Gen.oneOf[(String,String) => Boolean](
        (s1:String, s2:String) => s1.compareTo(s2) > 0 ,
        (s1:String, s2:String) => s1.hashCode > s2.hashCode,
        (s1:String, s2:String) => s1.length > s2.length
      )

    forAll  ((getStringArray, "array to sort"), (stringCompare, "compare fn"), minSuccessful(1000)) {
      ( as: Array[String], compareFn: (String,String) => Boolean )   =>
        val ourFn = exercise2.isSorted(as, compareFn)
        val refFn = as.sortWith(compareFn).sameElements(as)
        ourFn shouldEqual(refFn)
      }
  }

//  @Test def testSortingFail:Unit = {
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



}
