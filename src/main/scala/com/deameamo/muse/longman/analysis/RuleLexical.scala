package com.deameamo.muse.longman.analysis

import java.util.regex.Pattern

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.combinator.token.Tokens

object RuleLexical {

  def main(args: Array[String]) {
    val lexical = new RuleLexical
    var tokens = new lexical.Scanner("{ANY}$A *and {ANY}$B->{ANY}^A%B,VAR+A,VAR+B")
    var i = 0
    while (!tokens.atEnd) {
      println(tokens.first)
      tokens = tokens.rest
      i += 1
    }
    println("done")
  }

}

class RuleLexical extends Scanners with RegexParsers with RuleTokens {
  override type Elem = Char

  def token: Parser[Token] =
    (whitespace ?) ~>
      (
        symbolRegex ^^ Symbol
          | number ^^ Number
          | literal ^^ Literal
        )

  val number: Parser[String] = """[1-9]""".r

  val literal: Parser[String] = """[\p{javaUnicodeIdentifierPart}']+""".r

  val symbols = List("{", "}", "[", "]", "<", ">", "(=", "=)", "(*", "*)", "(~", "~)", "(", ")", "->", "`", "*", "$", "+", "^", "@", "=", "~", ">", ".", ",", "/", "|", ":", "#", "%")

  val symbolRegex: Regex = symbols.map(Pattern.quote).mkString("|").r

  def whitespaceChar: Parser[Char] = elem("space char", ch => {
    ch == 0
  })

  def whitespace: Parser[Any] = rep[Any](whitespaceChar)
}

trait RuleTokens extends Tokens {

  case class Symbol(chars: String) extends Token {
    override def toString: String = chars
  }

  case class Literal(chars: String) extends Token {
    override def toString: String = chars
  }

  case class Number(chars: String) extends Token {
    override def toString = s"#$chars"
  }

}
