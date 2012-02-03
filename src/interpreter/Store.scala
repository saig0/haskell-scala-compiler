package interpreter
import monad.Action._

object Store {

  class Addr(i: Int)

  type Store = Map[Addr, Val]

  def emptyStore: Store = Map[Addr, Val]()

  def insert(value: Val): Action[Addr] =
    new Action({ s =>
      val addr = new Addr(s.size)
      (s + (addr -> value), addr)
    })

  def get(addr: Addr): Action[Val] =
    new Action({ s =>
      (s, s(addr))
    })

  def put(addr: Addr, value: Val): Action[ValUnit] =
    new Action({ s =>
      (s.updated(addr, value), ValUnit())
    })
}