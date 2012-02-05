package interpreter

object Cps {

  type CPS = Cps[Val]

  implicit def val2Cps(value: Val): CPS = new Cps({ c => c(value) })

  // implicit def cps2Val(cps: CPS): Val = ident(cps)

  def id(c: (Val => Val) => Val): Val = c({ x => x })

  def ident(c: Cps[Val]): Val = c match { case Cps(c) => id(c) }
}

case class Cps[T](c: ((T => T) => T)) {

  def bind(f: (T => Cps[T])): Cps[T] =
    new Cps({ k =>
      c(k({ x: T => f(x) match { case Cps(r) => r } }))
    })

}
