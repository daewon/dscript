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

/**
  * Exp tree
  */
object ExpTree {

  object Helper {
    // helper functions for build Tree

    def FCall(ident: String, args: Exp*): EFCall = {
      EFCall(EIdent(ident), ESeq(args))
    }

    def Add(args: Exp*) = FCall("+", args: _*)

    def Sub(args: Exp*) = FCall("-", args: _*)

    def Mul(args: Exp*) = FCall("*", args: _*)

    def Div(args: Exp*) = FCall("/", args: _*)

    def And(args: Exp*) = FCall("&&", args: _*)

    def Or(args: Exp*) = FCall("||", args: _*)

    def In(args: Exp*) = FCall("in", args: _*)

    def Eq(args: Exp*) = FCall("==", args: _*)

    def Ne(args: Exp*) = FCall("!=", args: _*)
  }

  type ExpNumber = BigDecimal

  trait Exp extends Any

  trait ExpFn extends (ESeq => ExpResult) with Exp

  case class EFCall(name: EIdent, args: ESeq) extends Exp {
    override def toString = show(this)
  }

  case class EAssign(ident: EIdent, v: Exp) extends Exp {
  }

  case class ELambda(args: ESeq, body: Exp, env:  Option[EEnv]) extends Exp {
  }

  // TODO: eval order, toString
  case class EEnv(e: Exp, assigns: Seq[EAssign]) extends Exp {
  }

  //  case class EMap(vs: Map[String, Exp]) extends Exp {
  //  }

  case class ESeq(vs: Seq[Exp]) extends AnyVal with Exp {
    override def toString = vs.mkString("`(", " ", ")")
  }

  case class EIdent(v: String) extends AnyVal with Exp {
    def hierarchy = v.split("\\.").toSeq

    override def toString = v
  }

  case class ENum(v: ExpNumber) extends AnyVal with Exp {
    override def toString = v.toString
  }

  case class EStr(v: String) extends AnyVal with Exp {
    override def toString =
      if (v.contains("\\\"")) {
        s""""${v.replace("\"", "\\\"")}""""
      } else {
        s"""'${v.replace("'", "\\'")}'"""
      }
  }

  case object ENull extends Exp {
    override def toString = "null"
  }

  case object ETrue extends Exp {
    override def toString = "true"
  }

  case object EFalse extends Exp {
    override def toString = "false"
  }

  /**
    * repr tree as (s-exp)
    */
  def show(e: Exp, depth: Int = 0, pretty: Boolean = false): String = e match {
    case EFCall(sym, ESeq(args)) =>
      if (pretty) showRecur(s"${sym}", args, depth)
      else s"(${sym} ${args.mkString(" ")})"
    case _ =>
      if (pretty) indent(depth, e.toString) else e.toString
  }

  private def showRecur(sym: String, args: Seq[Exp], depth: Int, space: Int = 2): String = {
    val nextDepth = depth + space
    val argsStr = args.map { e => show(e, nextDepth + 1, pretty = true) }.mkString("\n")

    if (args.isEmpty) indent(depth, s"($sym)")
    else indent(depth, s"(${sym} \n${argsStr})")
  }

  private def indent(depth: Int, v: String, indentCh: String = " "): String =
    s"${indentCh * depth}${v}"
}
