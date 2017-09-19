/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.s2graph.core.parsers.Exp

import org.apache.s2graph.core.parsers.Exp.ExpEnv.CanFind

/**
  * Exp evaluator
  */
object ExpEvaluator {

  import org.apache.s2graph.core.parsers.Exp.ExpTree._

  object EitherOpts {
    def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
      s.foldRight(Right(Nil): Either[A, List[B]]) {
        (e, acc) => for {
          xs <- acc.right
          x <- e.right
        } yield x :: xs
      }
  }

  def eval[A: CanFind](exp: Exp, env: A): ExpResult = exp match {
    case EEnv(exp, Seq(assigns@_*)) =>
      val envVals = EitherOpts.sequence(assigns.map(e => eval(e, env)))
      envVals.fold(
        e => Left("error" :: e),
        seq => {
          val newEnv = assigns.map(_.ident.v).zip(seq).toMap
          eval(exp, ExpEnv(newEnv, parent = Option(env)))
        })

    case fc@EFCall(fn: EIdent, args@ESeq(_)) =>
      ExpEnv.findWith(env, fn.v).right.flatMap { case (f: ExpFn) =>
        eval(args, env).right.flatMap { case (args: ESeq) => f(args) }
      }

    case ESeq(seq) => EitherOpts.sequence(seq.map(e => eval(e, env))).fold(
      error => Left("error" :: error),
      seq => Right(ESeq(seq))
    )

    case EAssign(_, v) => eval(v, env)
    case EIdent(v) => ExpEnv.findWith(env, v)
    case _ => Right(exp)
  }
}
