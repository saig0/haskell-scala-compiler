package monad
import interpreter.Store._

trait Monad[M[_]] {
  def unit[A](a: A): M[A]
  def bind[A, B](m: M[A])(f: A => M[B]): M[B]

  def >>=[A, B](m: M[A])(f: A => M[B]) = bind(m)(f)
}

