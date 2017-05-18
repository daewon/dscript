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

import org.apache.s2graph.core.parsers.Exp.ExpEnv.CanFind
import org.apache.s2graph.core.parsers.Exp._
import org.apache.s2graph.core.parsers.Exp.ExpTree._
import org.scalatest.{FunSuite, Matchers}

class ExpEvaluatorTest extends FunSuite with Matchers {
  val parser = new ExpParser()
  val emptyEnv = Map.empty[String, Exp]

  def eval[A: CanFind](exp: String, env: A) = {
    val eline = " " * 80
    val sline = "_" * 80

    println(eline)
    println(s"<< $exp >>")
    println(s"$sline\n")
    val parsed = parser.parse(exp)
    parsed.left.foreach { l =>
      println(s"Failed to parse: ${l}")
    }
    parsed.right.flatMap { tree =>
      println(ExpTree.show(tree, pretty = true))

      val ret = ExpEvaluator.eval(tree, env)

      println(s"$sline\n")
      println(s">> $ret")
      println(s"$sline")

      ret
    }
  }

  test("simple expression") {
    val exp = """1 + 2 + 3""".stripMargin
    val ret = eval(exp, emptyEnv)

    assert(ret.isRight)
    assert(ret.right.get == ENum(6))
  }

  test("simple boolean expression 1") {
    val exp = """true && false || true""".stripMargin
    val ret = eval(exp, emptyEnv)

    assert(ret.isRight)
    assert(ret.right.get == ETrue)
  }

  test("simple boolean expression 2") {
    val exp = """1 * 2 && 3""".stripMargin
    val ret = eval(exp, emptyEnv)

    assert(ret.isRight)
    assert(ret.right.get == ENum(3))
  }

  test("simple boolean expression 3") {
    val exp = """true and false or 10""".stripMargin
    val ret = eval(exp, emptyEnv)

    assert(ret.isRight)
    assert(ret.right.get == ENum(10))
  }

  test("simple boolean expression 4") {
    val exp = """null or math.max(1, 2)""".stripMargin
    val ret = eval(exp, emptyEnv)

    assert(ret.isRight)
    assert(ret.right.get == ENum(2))
  }

  test("operator precedence") {
    val exp = """1 - 2 * 3""".stripMargin
    val ret = eval(exp, emptyEnv)

    assert(ret.isRight)
    assert(ret.right.get == ENum(-5))
  }

  test("operator precedence paren") {
    val exp = """(1 - 2) * 3""".stripMargin
    val ret = eval(exp, emptyEnv)

    assert(ret.isRight)
    assert(ret.right.get == ENum(-3))
  }

  test("operator precedence: `* is higher than `==)") {
    val exp = """(10 == age * 10) and (10 / age == 10)""".stripMargin
    val ret = eval(exp, emptyEnv + ("age" -> ENum(1)))

    assert(ret.isRight)
    assert(ret.right.get == ETrue)
  }

  test("func call") {
    val exp = """math.max(1, 2)""".stripMargin
    val ret = eval(exp, emptyEnv)

    assert(ret.isRight)
    assert(ret.right.get == ENum(2))
  }

  test("complex func call") {
    val exp = """math.min(math.max(1, 2), 1) * 10""".stripMargin
    val ret = eval(exp, emptyEnv)

    assert(ret.isRight)
    assert(ret.right.get == ENum(10))
  }

  test("value from env") {
    val exp = """(math.pi - 1) + age""".stripMargin
    val ret = eval(exp, emptyEnv + ("age" -> ENum(1)))

    assert(ret.isRight)
    assert(ret.right.get == ENum(3.14))
  }

  test("pipe sequence") {
    val exp = """0 |> math.max(2) |> math.min(0)""".stripMargin
    val ret = eval(exp, emptyEnv + ("age" -> ENum(1)))

    assert(ret.isRight)
    assert(ret.right.get == ENum(0))
  }

  test("string concat") {
    val exp = """ prefix + name + '!' + postfix """.stripMargin
    val ret = eval(exp, emptyEnv ++ Map("prefix" -> EStr("<< "), "postfix" -> EStr(" >>"), "name" -> EStr("graph")))

    assert(ret.isRight)
    assert(ret.right.get == EStr("<< graph! >>"))
  }

  test("booelan with infix op") {
    val exp = """1 in [age, 2] and true""".stripMargin
    val ret = eval(exp, emptyEnv + ("age" -> ENum(1)))

    assert(ret.isRight)
    assert(ret.right.get == ETrue)
  }

  test("with where clause") {
    val exp = """a * 1 where a = 1""".stripMargin

    val ret = eval(exp, emptyEnv)

    assert(ret.isRight)
    assert(ret.right.get == ENum(1))
  }

  test("with nested where clause") {
    val exp =
      """ a * b
        |   where
        |     a = (1),
        |     b = d
        |       where d = (3-1)""".stripMargin

    val ret = eval(exp, emptyEnv)

    assert(ret.isRight)
    assert(ret.right.get == ENum(2))
  }

  test("sequence literal literals") {
    val exp = """["string", 1, 2, math.min(3, 4), [1, 2]]""".stripMargin
    val ret = eval(exp, emptyEnv)

    val expected = ESeq(
      Seq(
        EStr("string"),
        ENum(1),
        ENum(2),
        ENum(3),
        ESeq(
          Seq(
            ENum(1),
            ENum(2)
          )
        )))

    assert(ret.isRight)
    assert(ret.right.get == expected)
  }
}

