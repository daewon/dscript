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
  * Function impl collections
  */
object ExpFn {

  import ExpTree._

  private def error(fname: String, args: ESeq): ExpResult =
    Left(List(s"Invalid Arguments: (${fname} ${args})"))

  def binaryOp(s: String)(f: (Exp, Exp) => Exp): ExpFn = new ExpFn {
    override def apply(args: ESeq) = args match {
      case ESeq(Seq(a, b)) => Right(f(a, b))
      case _ => error(s, args)
    }
  }

  object comp {
    val eq = binaryOp("==") { case (a, b) => if (a == b) ETrue else EFalse }
    val ne = binaryOp("!=") { case (a, b) => if (a != b) ETrue else EFalse }
    //    val gt = ???
    //    val lt = ???
    //    val gte = ???
    //    val lte = ???
  }

  object seq {
    val in = binaryOp("in") {
      case (a, b) =>
        if ((a != ENull && a != EFalse) && (b != ENull && b != EFalse)) b
        else EFalse
    }
  }

  object bool {
    val `and` = binaryOp("and") {
      case (a, b) =>
        if ((a != ENull && a != EFalse) && (b != ENull && b != EFalse)) b
        else EFalse
    }

    val `or` = binaryOp("or") {
      case (a, b) =>
        if (a != ENull && a != EFalse) a
        else if (b != ENull && b != EFalse) b
        else EFalse
    }
  }

  object math {
    val max = binaryOp("max") { case (ENum(a), ENum(b)) => if (a > b) ENum(a) else ENum(b) }
    val min = binaryOp("min") { case (ENum(a), ENum(b)) => if (a < b) ENum(a) else ENum(b) }

    // basic operators
    val add = binaryOp("+") {
      case (ENum(a), ENum(b)) => ENum(a + b)
      case (EStr(a), EStr(b)) => EStr(a + b)
    }

    val mul = binaryOp("*") {
      case (ENum(a), ENum(b)) => ENum(a * b)
      case (EStr(a), ENum(b)) => EStr(a * b.toInt)
    }

    val sub = binaryOp("-") { case (ENum(a), ENum(b)) => ENum(a - b) }
    val div = binaryOp("/") { case (ENum(a), ENum(b)) => ENum(a / b) }
  }

  object time {
    val now = new ExpFn {
      override def apply(_args: ESeq) = Right(ENum(System.currentTimeMillis()))
    }
  }

}
