package org.eoin

import org.eoin.Chapter11.{Monad, exercise1, exercise19, exercise7}
import org.junit.Test
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.language.higherKinds

/**
  * Created by eoin.parker on 11/10/16.
  */
class Chapter11TestSuite extends JUnitSuite with GeneratorDrivenPropertyChecks {

  def associativeLaw[M[_],A,B,C] (m:Monad[M])
                                 (implicit arbMA: Arbitrary[M[A]],
                                  arbFunc1: Arbitrary[A => M[B]], arbFunc2: Arbitrary[B => M[C]]) = {

    forAll (minSuccessful(1000)) { (ma: M[A], f:A=>M[B], g:B=>M[C]) =>
      val x1 = m.flatMap(ma)(f)
      val x2 = m.flatMap(x1)(g)  // f then g

      val y1 = (a:A) => m.flatMap(f(a))(g)
      val y2 =  m.flatMap(ma)(y1) // g then f

      x2 should equal(y2)
    }
  }


//  def zeroElement[A] (m:Monoid[A]) (implicit arbA: Arbitrary[A]) = {
//    forAll (minSuccessful(1000)) { (a:A) =>
//      //println(a)
//      m.op (a, m.zero)  should be (a)
//      m.op (m.zero, a)  should be (a)
//
//    }
//  }

  @Test def exercise3SequenceTraverse : Unit = {


    val findFn = (s:String) => s.find(_.isWhitespace)
    val foundYesNoFn = findFn andThen( _.isDefined)
    val mapFn = findFn andThen {_.map { Character.getNumericValue(_) }}

    val om = exercise1.optionMonad

    forAll  { (los:List[Option[String]]) =>
      val referenceResult = {
        val fs = los.filter { _.isDefined } map {_.get}
        if (fs.size == 0 ) None else Some (fs)
      }
      val ourResult = om.sequence(los)
      //ourResult should be (referenceResult)
    }

    forAll  { (ls:List[String]) =>
      val referenceResult = {
        val fs = ls.filter { foundYesNoFn }
        if (fs.size == 0 ) None else Some (fs)
      }
      val ourResult = om.traverse(ls) { mapFn }
      //ourResult should be (referenceResult)
    }

  }

    @Test def exercise7Expansions : Unit = {
      exercise7.genOrderExpanded.sample map {println}
      exercise7.genOrder2Expanded.sample map {println}
    }

    @Test def exercise8MonadAssociativity: Unit = {

     implicit val flatMapFnGenner1 : Arbitrary [Int => Option[Double]] = Arbitrary {
          val func = (i:Int) => {
            val s = Arbitrary.arbitrary[Double].sample
            s map {_ + i}
        }
          Gen.const(func)
        }

      implicit val flatMapFnGenner2 : Arbitrary [Double => Option[String]] = Arbitrary {
          val func = (d:Double) => {
            val s = Arbitrary.arbitrary[String].sample
            s map {_ + d}
        }
          Gen.const(func)
        }

      implicit val optionIntGenner : Arbitrary [Option[Int]] = Arbitrary {
        val int = new java.util.Random().nextInt()
        if (int % 2 == 0) Some(int) else None
      }
          associativeLaw[Option, Int, Double, String] ( exercise1.optionMonad ) (optionIntGenner, flatMapFnGenner1, flatMapFnGenner2)
    }


  @Test def exercise19IdMonad: Unit = {

    import exercise19._

    val x = "elephants!"

    val id1 = Id(3.14159)
    val id2 = Id("eoin")

    // try to pimp in the map/flatMap,  for the for-yield
    implicit class IdAsMonad[A] (val id: Id[A]) extends Monad[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)
      override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = f(ma.value)
    }

//    def doTest(implicit mi: Monad[Id]) = {
//      //val imp = implicitly[Monad[Id]]
//      val mid1 = mi.unit(id1)
//      val mid2 = mi.unit(id2)

    val id3 = for {
      v1 <- id1
      v2 <- id2
      v3 = s"$v2  $v1  :: $x"
    } yield v3.length

    //id3 map println

  }

}