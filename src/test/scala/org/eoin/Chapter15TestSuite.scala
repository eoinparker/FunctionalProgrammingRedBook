package org.eoin

import org.junit.Test
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.eoin.Chapter15._

/**
  * Created by eoin.parker on 1/13/17.
  */
class Chapter15TestSuite extends JUnitSuite with GeneratorDrivenPropertyChecks {


   @Test def exercise1 : Unit = {

     val p1 = Process.emit(Nil, Halt() )
  }

}
