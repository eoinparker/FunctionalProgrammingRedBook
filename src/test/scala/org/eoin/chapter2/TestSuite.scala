package org.eoin.chapter2

import org.junit.Test
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.Matchers._

/**
  * Created by eoin.parker on 11/10/16.
  */
class TestSuite extends JUnitSuite with GeneratorDrivenPropertyChecks {

  @Test def testFib : Unit = {

    forAll {   ( n: Int) =>
      whenever(n > 0 && n < Integer.MAX_VALUE) {
        val nth = exercise1.fib(n)
        val `n + 1'th` = exercise1.fib(n + 1)
        val `n + 2'th` = exercise1.fib(n + 2)

        `n + 2'th` should equal(nth + `n + 1'th`)
      }
    }
  }
}
