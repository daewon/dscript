package org.apache.s2graph.core.parsers.Exp

import fastparse.WhitespaceApi

/**
  * Simple expression parser
  */
class ExpParser {

  import ExpTree._
  import ExpTree.Helper._
  import fastparse.noApi._

  type PChar = (Char) => Boolean

  // predicates for WhiteSpace | Digit | StringChar
  val isWhiteSpace: PChar = ch => ch == '\n' || ch == '\r' || ch == ' '
  val isDigit: PChar = ('0' to '9').toSet
  val isStringChar: PChar = ch => ch != '\\' && ch != '\'' && ch != '\"'
  val isIdentChar: PChar = Set(('a' to 'z') ++ ('A' to 'Z') ++ "`_").flatten

  // It can ignore WhiteSpace between sequence parser
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharsWhile(isWhiteSpace).rep)
  }

  import White._

  val space = P(CharsWhile(isWhiteSpace).?)

  val digits = P(CharsWhile(isDigit))

  val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)

  val fractional = P("." ~ digits)

  val integral = P("0" | CharIn('1' to '9') ~ digits.?)

  val hexDigit = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))

  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)

  val escape = P("\\" ~ (CharIn("'\"/\\bfnrt") | unicodeEscape))

  val strChars = P(CharsWhile(isStringChar))

  val identSingle = P(!"-" ~ CharsWhile(isIdentChar).!)

  // Literals
  val `null`: P[ENull.type] = P("null").map(_ => ENull)

  val `true`: P[ETrue.type] = P("true").map(_ => ETrue)

  val `false`: P[EFalse.type] = P("false").map(_ => EFalse)

  val string: P[EStr] = P(P(CharIn("'\"")) ~/ (strChars | escape).rep.! ~ P(CharIn("'\""))).map { s =>
    EStr(s.replace("\\'", "'").replace("\\\"", "\""))
  }

  // IEEE Long format
  val number: P[ENum] = P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map {
    n => ENum(BigDecimal(n))
  }

  val ident: P[EIdent] = P(identSingle ~ ("." ~ identSingle).rep).map {
    case (ident, identLs) => EIdent(ident +: identLs mkString ".")
  }

  val funcCall: P[EFCall] = P(ident ~ "(" ~/ binOp2.rep(sep = ",") ~ ")").map {
    case (ident, exps) => EFCall(ident, ESeq(exps))
  }

  // binary ops
  val infixAdd = "+"
  val infixSub = "-"
  val infixMul = "*"
  val infixDiv = "/"

  val infixAnd = "and" | "&&"
  val infixOr = "or" | "||"

  val infixEq = "=="
  val infixNe = "!="

  val infixIn = "in"
  val infixPipe = "|>"

  val where = "where"

  // seq literal
  val seq: P[ESeq] = P("[" ~/ factor.rep(sep = ",") ~/ "]").map(ESeq)

  // paren (exp)
  val paren: P[Exp] = P("(" ~ binOp2 ~ ")" ~/ !"->").map(exp => exp)

  // define lambda
  val lambda: P[Exp] = P("(" ~/ ident.rep(sep=",") ~/ ")" ~/ "->" ~/ withEnv).map {
    case (args, exp) => ELambda(ESeq(args), exp, None)
  }

  // terminal
  val factor: P[Exp] = P(paren | number | string | `true` | `false` | `null` | lambda | seq | funcCall | ident)

  // high precedence binary op
  val binOp1: P[Exp] = P(factor ~ (P(infixMul | infixDiv | infixAnd | infixOr).! ~/ factor).rep).map {
    case (exp, opWithExp) => opWithExp.foldLeft(exp) {
      case (acc, ("and", exp)) => And(acc, exp)
      case (acc, ("&&", exp)) => And(acc, exp)
      case (acc, ("or", exp)) => Or(acc, exp)
      case (acc, ("||", exp)) => Or(acc, exp)
      case (acc, ("*", exp)) => Mul(acc, exp)
      case (acc, ("/", exp)) => Div(acc, exp)
    }
  }

  // low precedence binary op
  val binOp2: P[Exp] = P(binOp1 ~ (P(infixIn | infixEq | infixNe | infixAdd | infixSub).! ~/ binOp1).rep).map {
    case (exp, opWithExp) => opWithExp.foldLeft(exp) {
      case (acc, ("in", exp)) => In(acc, exp)
      case (acc, ("==", exp)) => Eq(acc, exp)
      case (acc, ("!=", exp)) => Ne(acc, exp)
      case (acc, ("+", exp)) => Add(acc, exp)
      case (acc, ("-", exp)) => Sub(acc, exp)
    }
  }

  val assign: P[EAssign] = P(ident ~/ "=" ~/ withEnv).map {
    case (ident: EIdent, exp) => EAssign(ident, exp)
  }

  val withEnv: P[Exp] = P(binOp2 ~ ("where" ~/ assign.rep(min = 1, sep = ",")).?).map {
    case (exp, Some(assigns)) => EEnv(exp, assigns)
    case (exp, None) => exp
  }

  // pipe: macro expansion
  val pipe: P[Exp] = P(withEnv ~ P(infixPipe ~ withEnv).rep).map {
    case (exp, exps) =>
      exps.foldLeft(exp) {
        case (acc, f@EFCall(name, ESeq(args))) =>
          f.copy(args = ESeq(acc +: args))
      }
  }

  // expression is seq of pipe
  val expr: P[Exp] = P(pipe ~ End)

  def parse(str: String): ExpResult = expr.parse(str) match {
    case Parsed.Success(tree, _) => Right(tree)
    case Parsed.Failure(expected, failIndex, extraInfo) =>
      Left(List(s"$expected: ${expected}, index: ${failIndex}, info${extraInfo}"))
  }
}
