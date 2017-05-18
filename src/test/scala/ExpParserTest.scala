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
package org.apache.s2graph.core.parsers

import org.apache.s2graph.core.parsers.Exp._
import org.apache.s2graph.core.parsers.Exp.ExpTree._
import org.scalatest.{FunSuite, Matchers}

class ExpParserTest extends FunSuite with Matchers {

  import ExpTree.Helper._

  val parser = new ExpParser()

  def parse(exp: String): Exp = {
    val eline = " " * 80
    val sline = "_" * 80

    val parsed = parser.parse(exp)

    println(eline)

    val ret = if (parsed.isRight) {
      println(s"<< $exp >>")
      println(s"$sline\n")
      val valid = parsed.right.get
      println(ExpTree.show(valid, pretty = true))
      println(s"$sline")

      parsed.right.get
    } else {
      println(parsed.left.get)
      ENull
    }

    ret
  }

  test("null literal") {
    val exp = """null""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree = ENull

    assert(parsedTree == expectedTree)
  }

  test("number literal") {
    val exp = """1.2e+10""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree = ENum(1.2e10)

    assert(parsedTree == expectedTree)
  }

  test("number literal 2") {
    val exp = """-124.5""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree = ENum(-124.5)

    assert(parsedTree == expectedTree)
  }

  test("string literal") {
    val exp = "'string'".stripMargin
    val exp2 = "\"string\"".stripMargin
    val (p, p2) = parse(exp) -> parse(exp2)
    val expectedTree = EStr("string")

    assert(p == expectedTree)
    assert(p2 == expectedTree)
  }

  test("string literal with escaped single quote") {
    val exp = "'str\\'ing'".stripMargin
    val parsedTree = parse(exp)
    val expectedTree = EStr("str'ing")

    assert(parsedTree == expectedTree)
    assert(parsedTree.asInstanceOf[EStr].v.length == 7)
  }

  test("string literal with escaped double quote") {
    val exp = "\"str\\\"ing\"".stripMargin
    val parsedTree = parse(exp)
    val expectedTree = EStr("str\"ing")

    assert(parsedTree == expectedTree)
    assert(parsedTree.asInstanceOf[EStr].v.length == 7)
  }

  test("basic expression") {
    val exp = """1 + 2""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree =
      Add(
        ENum(1.0),
        ENum(2.0))

    assert(parsedTree == expectedTree)
  }

  test("basic expression 2") {
    val exp = """-1--1""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree =
      Sub(
        ENum(-1),
        ENum(-1))

    assert(parsedTree == expectedTree)
  }

  test("operator precedence") {
    val exp = """1 - 2 * 3""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree =
      Sub(
        ENum(1.0),
        Mul(
          ENum(2.0),
          ENum(3.0)))

    assert(parsedTree == expectedTree)
  }

  test("operator precedence 2") {
    val exp = """1 * 2 * 3""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree =
      Mul(
        Mul(
          ENum(1.0),
          ENum(2.0)
        ),
        ENum(3.0)
      )

    assert(parsedTree == expectedTree)
  }

  test("operator precedence with paren") {
    val exp = """(1 - 2) * 3""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree =
      Mul(
        Sub(
          ENum(1.0),
          ENum(2.0)),
        ENum(3.0))

    assert(parsedTree == expectedTree)
  }

  test("identifier") {
    val exp = """`age_band""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree = EIdent("`age_band")

    assert(parsedTree == expectedTree)
  }

  test("identifier with dot") {
    val exp = """module.math . max""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree = EIdent("module.math.max")

    assert(parsedTree == expectedTree)
  }

  test("operation with identifier") {
    val exp = """age_band / 1000 * 1000""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree =
      Mul(
        Div(
          EIdent("age_band"),
          ENum(1000)),
        ENum(1000))

    assert(parsedTree == expectedTree)
  }

  test("func call") {
    val exp = """math.max(age_band, math.min(1000, 10))""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree =
      FCall("math.max",
        EIdent("age_band"),
        FCall("math.min",
          ENum(1000), ENum(10)))

    assert(parsedTree == expectedTree)
  }

  test("complex expr") {
    val exp = """(1 + 2) * math.max(age_band, math.min(1000, 10)) / age_band""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree =
      Div(
        Mul(
          Add(
            ENum(1),
            ENum(2)
          ),
          FCall("math.max",
            EIdent("age_band"),
            FCall("math.min",
              ENum(1000),
              ENum(10)
            )
          )
        ),
        EIdent("age_band")
      )

    assert(parsedTree == expectedTree)
  }

  test("empty sequence literal") {
    val exp = """[]""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree = ESeq(Nil)

    assert(parsedTree == expectedTree)
  }

  test("sequence literal") {
    val exp = """[1, 2]""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree = ESeq(Seq(ENum(1), ENum(2)))

    assert(parsedTree == expectedTree)
  }

  test("sequence literal literals") {
    val exp = """["string", 1, 2, math.min(3, 4), time.now(), [1, 2]]""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree = ESeq(
      Seq(
        EStr("string"),
        ENum(1),
        ENum(2),
        FCall("math.min", ENum(3), ENum(4)),
        FCall("time.now"),
        ESeq(Seq(ENum(1), ENum(2)))
      )
    )

    assert(parsedTree == expectedTree)
  }

  test("infix operator") {
    val exp = """1 in [1, 2]""".stripMargin
    val parsedTree = parse(exp)
    val expectedTree = In(
      ENum(1),
      ESeq(Seq(
        ENum(1),
        ENum(2)
      )))

    assert(parsedTree == expectedTree)
  }

  test("lambda") {
    val exp =
      """add(1, 2) where add = (a, b) -> (a + b) + 10""".stripMargin

    val parsedTree = parse(exp)
    val expectedTree =
      EEnv(
        FCall("add",
          ENum(1),
          ENum(2)),
        Seq(
          EAssign(
            EIdent("add"),
            ELambda(
              ESeq(
                Seq(
                  EIdent("a"),
                  EIdent("b")
                )
              ),
              Add(
                Add(
                  EIdent("a"),
                  EIdent("b")
                ), ENum(10)),
              None)
          )))

    assert(parsedTree == expectedTree)
  }

  test("where") {
    val exp =
      """ a * b
        |   where
        |     a = 1,
        |     b = d
        |       where d = 3-1""".stripMargin

    val parsedTree = parse(exp)
    val expectedTree =
      EEnv(
        Mul(EIdent("a"), EIdent("b")),
        Seq(
          EAssign(
            EIdent("a"),
            ENum(1)
          ),
          EAssign(
            EIdent("b"),
            EEnv(
              EIdent("d"),
              Seq(
                EAssign(EIdent("d"),
                  Sub(ENum(3), ENum(1)))
              )))))

    assert(parsedTree == expectedTree)
  }

  test("pipe macro") {
    val exp =
      """
        time.now()
        |> math.max(2)
        |> math.min(0)
      """ // with no .stripMargin

    val parsedTree = parse(exp)
    val expectedTree =
      FCall("math.min",
        FCall("math.max",
          FCall("time.now"),
          ENum(2)),
        ENum(0))

    assert(parsedTree == expectedTree)
  }
}
