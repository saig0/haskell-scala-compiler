package interpreter

import scala.collection.mutable.Map

object Store {

  class Addr(i: Int)

  type Store = Map[Addr, Val]

  def emptyStore: Store = Map[Addr, Val]()

  def insert(value: Val, c: (Addr => Val))(implicit store: Store) = {
    val addr = new Addr(store.size)
    store += (addr -> value)
    c(addr)
  }

  def get(addr: Addr, c: (Val => Val))(implicit store: Store) =
    c(store(addr))

  def put(addr: Addr, value: Val, c: (ValUnit => Val))(implicit store: Store) = {
    store(addr) = value
    c(ValUnit())
  }
}