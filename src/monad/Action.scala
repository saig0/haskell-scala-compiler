package monad
import interpreter.Store._

object Action {

  type A[T] = (Store => (Store, T))

  case class Action[T](a: A[T]) {
    def bind[B](f: A[T] => Action[B]): Action[B] = f(a)
  }

  object ActionMonad extends Monad[Action] {
    def unit[T](a: T) = new Action((s) => (s, a))
    def bind[T, B](a: Action[T])(f: T => Action[B]) =
      new Action({ s0 =>
        a match {
          case Action(a) => a(s0) match {
            case (s1, r1) => f(r1) match {
              case Action(b) => b(s1)
            }
          }
        }
      })
  }

}