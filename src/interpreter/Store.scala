package interpreter

import scala.collection.mutable.Map

object Store {

  class Addr(i: Int)

  type Store = Map[Addr, Val]

  def emptyStore: Store = Map[Addr, Val]()

  def insert(value: Val)(implicit store: Store): Addr = {
    val addr = new Addr(store.size)
    store += (addr -> value)
    addr
  }

  def get(addr: Addr)(implicit store: Store): Val = store(addr)

  def put(addr: Addr, value: Val)(implicit store: Store) {
    store(addr) = value
  }
}