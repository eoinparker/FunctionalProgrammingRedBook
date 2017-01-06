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
  * Created by eoin.parker on 11/10/16.
  */
class Chapter6TestSuite extends JUnitSuite with GeneratorDrivenPropertyChecks {

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

   @Test def exercise2BoundedDouble : Unit = {

     forAll (rngTestGen) { rngTestGenSample =>
       val (seed, timesToSampleRNG) = rngTestGenSample
       go(timesToSampleRNG, Set.empty, Chapter6.RNG.simple(seed))

       @tailrec
       def go (numTimes : Int, gennedPreviously: Set[Any], rng: RNG):RNG = {
        if (numTimes == 0 ) rng
        else {
          val (nextD, nextRNGState) = Chapter6.exercise2.double(rng)
          nextD should be >= 0.0
          nextD should be < 1.0
          gennedPreviously contains nextD should be (false)
          go(numTimes-1, gennedPreviously + nextD, nextRNGState)
        }
      }
     }
  }

   @Test def exercise3RandomTuples : Unit = {

     forAll (rngTestGen) { rngTestGenSample =>
       val (seed, timesToSampleRNG) = rngTestGenSample
       go(timesToSampleRNG, Set.empty, Chapter6.RNG.simple(seed))

       @tailrec
       def go (numTimes : Int, gennedPreviously: Set[Any], rng: RNG):RNG = {
        if (numTimes == 0 ) rng
        else {
          val ((i1,d1), nextRNGState1) = Chapter6.exercise3.intDouble(rng)
          val ((d2,i2), nextRNGState2) = Chapter6.exercise3.doubleInt(nextRNGState1)
          val ((d3,d4,d5), nextRNGState3) = Chapter6.exercise3.double3(nextRNGState2)
          List(d1,d2,d3,d4,d5) foreach { _ should be >= 0.0 }
          List(d1,d2,d3,d4,d5) foreach { _ should be < 1.0 }
          List(i1,i2) foreach { _ should be >= 0 }
          go(numTimes-1, gennedPreviously + Set(d1,d2,d3,d4,d5,i1,i2), nextRNGState3)
        }
      }
     }
  }


   @Test def exercise5PositiveMax : Unit = {

     forAll (rngTestGen, Gen.posNum[Int]) { (rngTestGenSample, maxRandInt) =>
       val (seed, timesToSampleRNG) = rngTestGenSample
       go(timesToSampleRNG, Set.empty, Chapter6.RNG.simple(seed))

       @tailrec
       def go (numTimes : Int, gennedPreviously: Set[Any], rng: RNG):RNG = {
        if (numTimes == 0 ) rng
        else {
          val (nextI, nextRNGState) = Chapter6.exercise5.positiveMax(maxRandInt) (rng)
          nextI should be >= 0
          gennedPreviously contains nextI should be (false)
          go(numTimes-1, gennedPreviously + nextI, nextRNGState)
        }
      }
     }
  }

     @Test def exercise6ReimplementedDouble : Unit = {

     forAll (rngTestGen) { rngTestGenSample =>
       val (seed, timesToSampleRNG) = rngTestGenSample
       go(timesToSampleRNG, Set.empty, Chapter6.RNG.simple(seed))

       @tailrec
       def go (numTimes : Int, gennedPreviously: Set[Any], rng: RNG):RNG = {
        if (numTimes == 0 ) rng
        else {
          val (nextD, nextRNGState) = Chapter6.exercise6.double(rng)
          nextD should be >= 0.0
          nextD should be < 1.0
          gennedPreviously contains nextD should be (false)
          go(numTimes-1, gennedPreviously + nextD, nextRNGState)
        }
      }
     }
  }


   @Test def exercise7IntDoubleReimplemented : Unit = {

     forAll (rngTestGen) { rngTestGenSample =>
       val (seed, timesToSampleRNG) = rngTestGenSample
       go(timesToSampleRNG, Set.empty, Chapter6.RNG.simple(seed))

       @tailrec
       def go (numTimes : Int, gennedPreviously: Set[Any], rng: RNG):RNG = {
        if (numTimes == 0 ) rng
        else {
          val ((i1,d1), nextRNGState1) = Chapter6.exercise7.intDouble (rng)
          val ((d2,i2), nextRNGState2) = Chapter6.exercise7.doubleInt (nextRNGState1)
          List(d1,d2) foreach { _ should be >= 0.0 }
          List(d1,d2) foreach { _ should be < 1.0 }
          List(i1,i2) foreach { _ should be >= 0 }
          go(numTimes-1, gennedPreviously + Set(d1,d2,i1,i2), nextRNGState2)
        }
      }
     }
  }


    @Test def exercise9PositiveInt : Unit = {

     forAll (rngTestGen) { rngTestGenSample =>
       val (seed, timesToSampleRNG) = rngTestGenSample
       go(timesToSampleRNG, Set.empty, Chapter6.RNG.simple(seed))

       @tailrec
       def go (numTimes : Int,gennedPreviously: Set[Any],  rng: RNG):RNG = {
        if (numTimes == 0 ) rng
        else {
          val (nextI, nextRNGState) = Chapter6.exercise9.positiveInt(rng)
          nextI should be >= 0
          gennedPreviously contains nextI should be (false)
          go(numTimes-1, gennedPreviously + nextI, nextRNGState)
        }
      }
     }
  }


      @Test def exercise10ReimplementedMap : Unit = {

     forAll (rngTestGen) { rngTestGenSample =>
       val (seed, timesToSampleRNG) = rngTestGenSample
       go(timesToSampleRNG, Set.empty, Chapter6.RNG.simple(seed))

       @tailrec
       def go (numTimes : Int, gennedPreviously: Set[Any], rng: RNG):RNG = {
        if (numTimes == 0 ) rng
        else {
          val (nextD, nextRNGState) = Chapter6.exercise10.double(rng)
          nextD should be >= 0.0
          nextD should be < 1.0
          gennedPreviously contains nextD should be (false)
          go(numTimes-1, gennedPreviously + nextD, nextRNGState)
        }
      }
     }
  }


   @Test def exercise10Map2Reimplemented : Unit = {

     forAll (rngTestGen) { rngTestGenSample =>
       val (seed, timesToSampleRNG) = rngTestGenSample
       go(timesToSampleRNG, Set.empty[Any], Chapter6.RNG.simple(seed))

       @tailrec
       def go (numTimes : Int, gennedPreviously: Set[Any], rng: RNG):RNG = {
        if (numTimes == 0 ) rng
        else {
          val ((i1,d1), nextRNGState1) = Chapter6.exercise10.intDouble (rng)
          val ((d2,i2), nextRNGState2) = Chapter6.exercise10.doubleInt (nextRNGState1)
          List(d1,d2) foreach { d =>
            d should be >= 0.0
            d should be < 1.0
            gennedPreviously contains d should be (false)
          }
          List(i1,i2) foreach { i =>
            i should be >= 0
            gennedPreviously contains i should be (false)
          }
          go(numTimes-1, gennedPreviously ++ Set(i1,i2,d1,d2), nextRNGState2)
        }
      }
     }
  }

   @Test def exerciseTestCoinStateMonad : Unit = {

     val inputGen = Gen.nonEmptyContainerOf[List, Input](Gen.oneOf(Coin,Turn))
     forAll(inputGen, Gen.posNum[Int], Gen.posNum[Int], minSuccessful(100)) { (inputs, initialCandies, initialCoins) =>
       val outputState = simulateMachine( inputs)
       val initialMachine = Machine(true, initialCandies, initialCoins)
       val (outputMachine,coinCount) = outputState.run(initialMachine)
       println(s"outputMachine ${outputMachine} ${coinCount} ${outputState}")

     }
   }


}
