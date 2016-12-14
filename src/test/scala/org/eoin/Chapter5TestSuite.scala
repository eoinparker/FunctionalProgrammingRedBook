package org.eoin

import Chapter5.Exercise1.MyStream
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
class Chapter5TestSuite extends JUnitSuite with GeneratorDrivenPropertyChecks {

  implicit def arbitraryMyStream[A] (implicit arbA: Arbitrary[A]) : Arbitrary[MyStream[A]] =
    Arbitrary {
      val mysGen: Gen[MyStream[A]] = for {
        listOfAs <- Gen.listOf(arbA.arbitrary)
      } yield MyStream(listOfAs: _*)
      mysGen
    }

  @Test def testCons : Unit = {
    forAll (minSuccessful(100)) { (mysi: MyStream[String], s:String) =>
      MyStream.cons(s, mysi).uncons should equal( Option(s,mysi))
    }
  }

  @Test def fails : Unit = {
    val mys = MyStream(1,2)
    mys.streamToList should equal (List(1,2))
  }

  @Test def exercise1StreamToList : Unit = {

    forAll { (ints: List[Int]) =>
      val mys = MyStream(ints:_*)
      mys.streamToList should equal (ints)
    }
  }

  @Test def exercise2TakeN : Unit = {

    forAll { (mysi: MyStream[Int], n:Int) =>
      whenever(n >=0) {
        mysi.take(n).streamToList should equal (mysi.streamToList.take(n))
      }
    }
  }

  @Test def exercise3TakeWhile : Unit = {

    // a random Int=>Boolean function that will return 'true' 99% of the time
    implicit def predicate = Arbitrary {
      Gen.function1[Int,Boolean] { Gen.frequency[Boolean]((99, true), (1, false)) }
    }

    forAll (minSuccessful(1000)) { (mysi: MyStream[Int], p: Int=>Boolean) =>
      val pass1 = mysi.takeWhile(p)
      val pass2 = pass1.takeWhile(p)
      pass1.streamToList should equal (pass2.streamToList)

    }
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
