import org.eoin.Chapter5.Exercise1.MyStream
import org.eoin.Chapter5.Exercise1.MyStream._

val ones : MyStream[Int] = cons (1, ones)

ones.take(5).streamToList

val f = MyStream.from(10).take(23)
f.streamToList

Range(1,2).toList

1 to 0

Int.MinValue