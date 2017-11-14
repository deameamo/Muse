package com.deameamo.longman.analysis0406

import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.token.Tokens
import java.util.regex.Pattern

object RuleLexical {
  
  def main(args: Array[String]) {
    val lexical = new RuleLexical
    var tokens = new lexical.Scanner("{ANY}$A *and {ANY}$B->{ANY}^A%B,VAR+A,VAR+B")
    var i = 0
    while(!tokens.atEnd) {
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
    (whitespace?) ~>
    (
      symbolRegex ^^ {Symbol(_)}
      |number ^^ {Number(_)}
      |literal ^^ {Literal(_)}
    )
  
  val number: Parser[String] = """[1-9]""".r
  
  val literal: Parser[String] = """[\p{javaUnicodeIdentifierPart}']+""".r
  
  val symbols = List("{", "}", "[", "]", "<", ">", "(=", "=)", "(*", "*)", "(~", "~)", "(", ")", "->", "`", "*", "$", "+", "^", "@", "=", "~", ">", ".", ",", "/", "|", ":", "#", "%")
  
  val symbolRegex = symbols.map(Pattern.quote(_)).mkString("|").r
  
  def whitespaceChar = elem("space char", ch => {ch == 0})
  
  def whitespace: Parser[Any] = rep[Any](whitespaceChar)
}

trait RuleTokens extends Tokens {
  
  case class Symbol(chars: String) extends Token {
    override def toString = chars
  }
  
  case class Literal(chars: String) extends Token {
    override def toString = chars
  }
  
  case class Number(chars: String) extends Token {
    override def toString = s"#${chars}"
  }
}