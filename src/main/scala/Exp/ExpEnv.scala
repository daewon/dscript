package org.apache.s2graph.core.parsers.Exp

import org.apache.s2graph.core.parsers.Exp.ExpEnv.CanFind

case class ExpEnv[A: CanFind, B: CanFind](env: A, parent: Option[B] = None)

object ExpEnv {

  import ExpTree._

  /**
    * Env for lookup symbol
    *
    * @tparam A
    */
  trait CanFind[A] {
    def find(env: A, key: String): ExpResult
  }

  val StdLib: Map[String, Exp] = Map(
    "+" -> ExpFn.math.add,
    "-" -> ExpFn.math.sub,
    "*" -> ExpFn.math.mul,
    "/" -> ExpFn.math.div,

    "&&" -> ExpFn.bool.`and`,
    "||" -> ExpFn.bool.`or`,

    "in" -> ExpFn.seq.in,

    "==" -> ExpFn.comp.eq,
    "!=" -> ExpFn.comp.ne,
    //    "gt" -> ExpFn..in,
    //    "lt" -> ExpFn..in,
    //    "gte" -> ExpFn.std.in,
    //    "lte" -> ExpFn.std.in,

    "math.max" -> ExpFn.math.max,
    "math.min" -> ExpFn.math.min,
    "math.pi" -> ENum(3.14),

    "time.now" -> ExpFn.time.now
  )

  def findWith[A: CanFind](env: A, name: String): ExpResult = {
    val findEither = implicitly[CanFind[A]].find(env, name)

    findEither.fold(
      _ => StdLib.get(name).toRight(List(s"can not find: ${name}")),
      exp => Right(exp)
    )
  }

  /**
    * Find exp value from Map[String, Exp]
    */
  implicit val mapCanFind = new CanFind[Map[String, Exp]] {
    override def find(env: Map[String, Exp], key: String): ExpResult =
      env.get(key).toRight(List(s"can not find: ${key}"))
  }

  implicit def envCanFind[A: CanFind, B: CanFind] = new CanFind[ExpEnv[A, B]] {
    override def find(env: ExpEnv[A, B], key: String): ExpResult = {
      val error: ExpResult = Left(List(s"can not find: ${key}"))

      implicitly[CanFind[A]].find(env.env, key).fold(
        e => env.parent.fold(error)(penv => implicitly[CanFind[B]].find(env.parent.get, key)),
        exp => Right(exp)
      )
    }
  }

  /**
    * Find exp value from Edge
    */
  //  implicit val edgeCanFind = new CanFind[Edge] {
  //    override def find(env: Edge, key: String): ExpResult = ???
  //  }

}
