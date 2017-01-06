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
import scala.collection.mutable.ListBuffer

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


  @Test def exercise1StreamToList : Unit = {

    forAll { (ints: List[Int]) =>
      val mys = MyStream(ints:_*)
      mys.streamToList should equal (ints)
    }
  }

  @Test def exercise2TakeN : Unit = {

    forAll (arbitraryMyStream[Int].arbitrary, Gen.choose(0,10), minSuccessful (100))
      { (mysi: MyStream[Int], n:Int ) =>
          mysi.take(n).streamToList should equal (mysi.streamToList.take(n))
      }

    forAll (arbitraryMyStream[Int].arbitrary, Gen.choose(0,100), minSuccessful (100))
      { (mysi: MyStream[Int], n:Int ) =>
          mysi.take(n).streamToList should equal (mysi.streamToList.take(n))
      }
  }

  @Test def exercise11FromN : Unit = {

    forAll (Arbitrary.arbInt.arbitrary, Gen.choose(0, 100), minSuccessful(1000)) { ( n:Int, count:Int ) =>
      println (s"n:$n count:$count")
          MyStream.fromUnfold(n).take(count).streamToList should equal((n to (n + count -1)).toList)
      }
  }

  @Test def exercise11ConstantN : Unit = {

    forAll (Arbitrary.arbInt.arbitrary, Gen.choose(0, 100), minSuccessful(1000))
      { ( n:Int, count:Int ) =>
          MyStream.constantUnfold(n).take(count).streamToList should equal(List.fill(count)(n))
      }
  }

  @Test def fails : Unit = {
    val v1 = MyStream.fromUnfold(2147483647).take(3).streamToList
    val v2 = (2147483647 to (2147483647 + 3 -1)).toList
    println (v1 ++ v2)
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

      val lb = ListBuffer[Int] ()
      val lessThan100 = mysi.takeWhile((i:Int) => {lb += i ; i < 10})
      lb.size should (equal(0) or equal(1))
      val asList = lessThan100.streamToList // this should force a lazy deref
      lb.size should equal(asList.size)

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
