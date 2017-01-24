package org.eoin

import org.eoin.Chapter6._
import org.junit.Test
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.annotation.tailrec

/**
  * Created by eoin.parker on 1/13/17.
  */
class Chapter13TestSuite extends JUnitSuite with GeneratorDrivenPropertyChecks {

   val rngTestGen = for {
     seed <- arbitrary[Int]
     timesToSampleRNG <- Gen.choose(1000,2000)
   }  yield ( seed, timesToSampleRNG)


   @Test def exercise1PositiveInt : Unit = {

     forAll (rngTestGen) { rngTestGenSample =>
       val (seed, timesToSampleRNG) = rngTestGenSample
       go(timesToSampleRNG, Set.empty, Chapter6.RNG.simple(seed))

       @tailrec
       def go (numTimes : Int, gennedPreviously: Set[Any], rng: RNG):RNG = {
        if (numTimes == 0 ) rng
        else {
          val (nextI, nextRNGState) = Chapter6.exercise1.positiveInt(rng)
          nextI should be >= 0
          gennedPreviously contains nextI should be (false)
          go(numTimes-1, gennedPreviously + nextI, nextRNGState)
        }
      }
     }
  }

}
