//import org.eoin.Chapter13.exercise5.{Bind, Done, More}
//
//object exercise5 {
//    sealed trait Trampoline[+A]
//    case class Done[+A](get: A) extends Trampoline[A]
//    case class More[+A](force: () => Trampoline[A]) extends Trampoline[A]
//    case class Bind[A,+B](force: () => Trampoline[A],
//                      f: A => Trampoline[B]) extends Trampoline[B]
//
//       def run[A](t: Trampoline[A]): A = t match {
//      case Done(get) => get
//      case More (force) => run(force())
//      case Bind(force: () => Trampoline[A], f) => {
//        val rf = run(force())
//      }
//    }
//  }
//  }
