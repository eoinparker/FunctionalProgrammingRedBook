

import org.eoin.Chapter14.{RunnableST, ST}
import org.eoin.STHashMap

val t = STHashMap(Map(1->"e", 2->"f"))

val updateActions = new RunnableST[Unit] {
  override def apply[S] = {
    val p = for {
      maybeX <- t.get(1)
      maybeY <- t.get(2)
      _ <-  for {
        x <- maybeX
        y <- maybeY
        _ <- t.put(3, x+y) } yield ST[_,Unit]()
      t.print
    } yield ()
  }

}

val o = ST.runST(updateActions)


