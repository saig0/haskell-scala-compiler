package monad
import interpreter.Store._

object Action {

  def run[T](a: Action[T]) =
    a match {
      case Action(f) =>
        f(emptyStore) match { case (_, r) => r }
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

import monad.Action._

case class Action[T](a: (Store => (Store, T))) {
  def >>=[B](f: T => Action[B]) = ActionMonad.bind(this)(f)
}