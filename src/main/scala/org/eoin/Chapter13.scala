//package org.eoin
//
//import org.eoin.Chapter11.Monad
//
//import scala.annotation.tailrec
//
//
///**
//  * Created by eoin.parker on 1/13/17.
//  */
//object Chapter13 {
//
////  object PastedFromBookText_Ignore {
////    trait IO[F[_], I, +A]
////
////    case class Pure[F[_], I, +A](get: A) extends IO[F,I,A]
////
////    case class Request[F[_], I, +A](
////      expr: F[I],
////      receive: (I) => IO[F, I, A] )  extends IO[F,I,A]
////
////    trait Console[A]
////    case object ReadLine extends Console[Option[String]]
////    case class PrintLine(s: String) extends Console[Unit]
////
////
////    trait Run[F[_]] {
////     def apply[A](expr: F[A]): (A, Run[F])
////    }
////
////    object IO {
////      @annotation.tailrec
////      def run[F[_],I,A](R: Run[F])(io: IO[F,I,A]): A = io match {
////        case Pure(a:A) => a
////        case Request(expr:F[I],recv: Function1[I,IO[F,I,A]]) => {
////          R(expr) match {
////            case (e,r2) => run(r2)(recv(e))
////          }
////        }
////      }
////    }
////
////    trait RunConsoleMock[F[_]] extends Run[Console[F]] {
////      def apply(c: Console[Option[String]]) = c match {
////        case ReadLine => (Some("Hello world!"), this)
////        case PrintLine(_) => ((), this)
////      }
////    }
////
////    trait RunConsole[F[_]] extends Run[Console[F]] {
////      def apply(c: Console) = c match {
////        case ReadLine =>
////          val r = try Some(readLine) catch { case _ => None }
////          (r, this)
////        case PrintLine(s) => (println(s), this)
////      }
////    }
////  }
//
//  // the real defn
////  sealed trait IO[F[_],A] { self =>
////    def run: A
////    def map[B](f: A => B): IO[F,B] = unit(f(run) )
////    def flatMap[B](f: A => IO[F,B]): IO[F,B] = f(run)
////    def unit[A](a: => A): IO[F,A] = new IO[F,A] { override def run: A = a }
////
////  }
////
////  case class Pure[A](a:  A) extends IO[A] {
////    override def run: A = a
////  }
////
////  case class Request[F[_] : Run,A] (expr: F[A], recvFn: A => (A, F[A]) ) extends IO[F,A] {
////    override def run: A = {
////      val runIt = implicitly[Run[F,A]].apply(expr)
////      runIt._1   // TODO ok to discard _2 ?
////    }
////  }
//
////   sealed trait IO[A] { self =>
////    def run: A
////    def map[B](f: A => B): IO[B] =
////      new IO[B] { def run = f(self.run) }
////    def flatMap[B](f: A => IO[B]): IO[B] =
////      new IO[B] { def run = f(self.run).run }
////  }
//
//  trait IO[F[_], +A]
//  case class Pure[F[_], +A](get: A) extends IO[F,A]
//  case class Request[F[_],I,+A](
//    expr: F[I],
//    receive: I => IO[F,A]) extends IO[F,A]
//  case class BindMore[F[_],A,+B](
//    force: () => IO[F,A],
//    f: A => IO[F,B]) extends IO[F,B]
//  case class BindRequest[F[_],I,A,+B](
//    expr: F[I], receive: I => IO[F,A],
//    f: A => IO[F,B]) extends IO[F,B]
//  case class More[F[_],A](force: () => IO[F,A]) extends IO[F,A]
//
//  type PartiallyFixedMonad[F] = Monad[({ type f[a] = IO[F,a]})#f]
//
//  object IO {
//    //def run[F[_],A](R: Run[F])(io: IO[F,A]): A = ???
//    def run[F[_],A](F: Monad[F])(io: IO[F,A]): F[A] = {
//      io match {
//        case Pure(get) => F.unit(get)
//        case Request(expr, receive) =>
//        case BindMore(force,f) => F.flatMap(force())(f)
//        case BindRequest(expr,f,receive) =>
//        case More(force) =>
//      }
//    }
//
//  }
//
//  def monad[F[_]] = new PartiallyFixedMonad[F] {
////      override def unit[A](a: => A): IO[F, A] = new IO[F,A] { override def run: A = a }
////      override def flatMap[A, B](ma: IO[F, A])(f: (A) => IO[F, B]): IO[F, B] = ma flatMap(f)
//    override def unit[A](a: => A): IO[F, A] = ???
//
//    override def flatMap[A, B](ma: IO[F, A])(f: (A) => IO[F, B]): IO[F, B] = ???
//  }
//
//
//  object IO extends PartiallyFixedMonad[IO] {
//    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
//    def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
//    def apply[A](a: => A): IO[A] = unit(a) // syntax for IO { .. }
//
//    def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
//    sealed class IORef[A](var value: A) {
//      def set(a: A): IO[A] = IO { value = a; a }
//      def get: IO[A] = IO { value }
//      def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
//    }
//  }
//
//
//  import scala.language.higherKinds
//
//    trait Run[F[_], A] {
//     def apply(expr: F[A]): (A, Run[F, A])
//    }
//
//    trait Console[A]
//    case object ReadLine extends Console[Option[String]]
//    case class PrintLine(s: String) extends Console[Unit]
//
//
//    //type FixedMonad[F] = Monad[({ type f[a] = IO[F,a]})#f]
//
//  object exercise1 {
//
//    def monad[F[_]] = new Monad[({ type f[a] = IO[F,a]})#f] {
//      override def unit[A](a: => A): IO[F, A] = new IO[F,A] { override def run: A = a }
//      override def flatMap[A, B](ma: IO[F, A])(f: (A) => IO[F, B]): IO[F, B] = ma flatMap(f)
//    }
//
//  }
//
//  object exercise2 {   //typechecks OK but seems 100% bananas
//      //@tailrec
//    def console(lines: List[String]) :Run[Console, Option[String]] = new Run[Console, Option[String]] { self =>
//      override def apply(expr: Console[Option[String]]) : (Option[String], Run[Console, Option[String]])= {
//        if (lines.isEmpty) (None,self)
//        else
//        expr match {
//          case PrintLine(_) => null  //ignore TODO
//          case ReadLine => (Some(lines.head), console(lines.tail))
//        }
//      }
//    }
//
//  }
//
//  object exercise3 {
//
//    def run[F[_],A](F: Monad[F])(io: IO[F,A]): F[A] = io match {
//      case Pure(a:A) => F.unit(a)
//      case Request(expr, receive) => F.flatMap(expr) { (a:A) => receive(a)._2 }  //discard the _1
//    }
//  }
//
//
//  object exercise4 {
//
//    import exercise1._
//    val F = monad[java.lang.Runnable]
//
//    val ex1 = F.sequence(List.fill(100000)(IO { math.random }))
//  }
//
//
//  object exercise5 {
//    sealed trait Trampoline[+A]
//    case class Done[+A](get: A) extends Trampoline[A]
//    case class More[+A](force: () => Trampoline[A]) extends Trampoline[A]
//    case class Bind[A,+B](force: () => Trampoline[A],
//                      f: A => Trampoline[B]) extends Trampoline[B]
//
//    @tailrec
//  def run[A](t: Trampoline[A]): A = t match {
//    case Done(get) => get
//    case More (force) => run(force())
//    case Bind(force: Function0[Trampoline[A]], f ) => run(force())
//    }
//  }
//
//  object exercise6 {
//
//    import exercise5._
//
//    object TrampolineMonad extends Monad[Trampoline] {
//      override def unit[A](a: => A): Trampoline[A] = Done(a)
//
//      override def flatMap[A, B](ma: Trampoline[A])(f: (A) => Trampoline[B]): Trampoline[B] = {
//        ma match {
//          case Done(get) => f(get)
//          case More (force) => Bind(force, f)
//          case Bind(force: Function0[Trampoline[A]], bindF: Function1[A,Trampoline[B]] ) => Bind(force, bindF andThen f) // compiles but seems wrong
//        }
//      }
//    }
//
//  }
//
