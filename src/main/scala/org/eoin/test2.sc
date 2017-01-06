import org.eoin.Chapter6.State

sealed trait Direction
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

case class WeatherVane (dir: Direction)

val WindDirs : Map[Int,Direction] =
  Map( 0 -> North, 1-> East, 2-> South, 3 -> West )

val wv1 = new State[Boolean, WeatherVane] ( (wv:WeatherVane) =>  {
  val currentWindDir = WindDirs.getOrElse(System.currentTimeMillis.toInt % 4, North)
  val windChanged = currentWindDir == wv.dir
  (windChanged, WeatherVane(currentWindDir))
  }
)


//val x = wv1.flatMap( _ match {
//  case true => new State
//  case false =>
//})

