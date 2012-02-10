package monad
import interpreter.Store._

trait Monad[M[_]] {
  def unit[A](a: A): M[A]
  def bind[A, B](m: M[A])(f: A => M[B]): M[B]

  implicit def monadSyntax[A, M[A]](a: M[A])(implicit m: Monad[M]) = new {
    def >>=[B](f: A => M[B]) = m.bind(a)(f)

    def foreach[B](f: A => M[B]) = >>=(f)
  }

}

