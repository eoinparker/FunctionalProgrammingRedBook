package org.eoin

import org.eoin.Chapter14.{ST, _}
import org.junit.Test
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks


/**
  * Created by eoin.parker on 1/13/17.
  */
class Chapter14TestSuite extends JUnitSuite with GeneratorDrivenPropertyChecks {

   val rngTestGen = for {
     seed <- arbitrary[Int]
     timesToSampleRNG <- Gen.choose(1000,2000)
   }  yield ( seed, timesToSampleRNG)


   @Test def exercise1 : Unit = {

     val x = for {
      r1 <- STRef[Nothing,Int](1)
      r2 <- STRef[Nothing,Int](1)
      x  <- r1.read
      y  <- r2.read
      _  <- r1.write(y+1)
      _  <- r2.write(x+1)
      a  <- r1.read
      b  <- r2.read
    } yield (a,b)

    println(s"x:$x")

  }

  @Test def exercise2 : Unit = {

    def getIntSTRef[S](i:Int) = STRef[S,Int](i)

    val p = new RunnableST[(Int, Int)] {
      def apply[S] = for {
        r1 <- getIntSTRef(1)
        r2 <- getIntSTRef(2)
        x  <- r1.read
        y  <- r2.read
        _  <- r1.write(y+1)
        _  <- r2.write(x+1)
        a  <- r1.read
        b  <- r2.read
      } yield (a,b)
    }

    val (a,b) = ST.runST(p)

    (a,b) should equal (3,2)


//    def impossible[S] = new RunnableST[STRef[S,Int]] {
//      def apply[S]  = {
//        val str = STRef[S,Int](1)
//        str
//      }
//    }
//    val i = impossible[Int]

    }

    @Test def exercise3 : Unit = {

      quicksort(List (5,6,4,3,2)) should equal (List(2,3,4,5,6))

      def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
        val arr = xs.toArray
        def swap(x: Int, y: Int) = {
          val tmp = arr(x)
          arr(x) = arr(y)
          arr(y) = tmp
        }
        def partition(l: Int, r: Int, pivot: Int) = {
          val pivotVal = arr(pivot)
          swap(pivot, r)
          var j = l
          for (i <- l until r) {
            if (arr(i) < pivotVal) {
              swap(i, j)
              j += 1
            }
          }
          swap(j, r)
          j
        }
        def qs(l: Int, r: Int): Unit = if (l < r) {
          val pi = partition(l, r, l + (r - l) / 2)
          qs(l, pi - 1)
          qs(pi + 1, r)
        }

        qs(0, arr.length - 1)
        arr.toList
      }
    }

  @Test def exercise4 : Unit = {

    case class Dog (name:String)
    def dogArr[S] =  STArray[S,Dog](5, new Dog("rover"))

    val actions = new RunnableST[List[Dog]] {
       def apply[S] = for {
         std <- ST[S,STArray[S,Dog]] {  dogArr }
          dog0 <- std.read(0)
          dog1 <- std.read(1)
          newDog = dog0.copy(name = dog0.name + dog1.name)
          _ <- dogArr.write(2, newDog)
         list <- dogArr.freeze
        } yield ( list )
    }

    val output = ST.runST(actions)

    output should equal (Dog("rover123"))
  }

  @Test def exercise5HashMap : Unit = {

    case class Dog (name:String)

    val initialInput = Map(1->Dog("rover"), 2->Dog("fido"), 3->Dog("spot"))
    val expectedOutput = Map(1->Dog("rover"), 2->Dog("fido"), 3->Dog("roverfido"))

    def dogMap[S] = STHashMap[S,Int,Dog](initialInput )

    val updateActions = new RunnableST[Map[Int,Dog]] {
      override def apply[S] = for {
          stmap <- ST[S,STHashMap[S,Int,Dog]] { dogMap }

          maybeVal1 <- stmap.get(1)
          maybeVa21 <- stmap.get(2)

          maybeUpdateAction = for {
              val1 <- maybeVal1
              val2 <- maybeVa21
            } yield { stmap.put(3, Dog(val1.name + val2.name)) }

          updateAction = maybeUpdateAction getOrElse {ST[S,Option[Dog]] (None)}
          u <- updateAction
          _ <- stmap.print
          s <- stmap.toImmutableMap
        } yield {s}
      }

    val s = ST.runST(updateActions)

    s should equal (expectedOutput)


  }

}
