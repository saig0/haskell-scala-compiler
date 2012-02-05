package compiler

import scala.collection.mutable.Map
import types.Type

object Store {

  class Addr(i: Int)

  type Store[T] = Map[Addr, T]

  def insert[T](value: T, c: (Addr => T))(implicit store: Store[T]) = {
    val addr = new Addr(store.size)
    store += (addr -> value)
    c(addr)
  }

  def get[T, S <: T](addr: Addr, c: (T => S))(implicit store: Store[T]) =
    c(store(addr))

  def put[T](addr: Addr, value: T, c: (() => T))(implicit store: Store[T]) = {
    store(addr) = value
    c()
  }
}