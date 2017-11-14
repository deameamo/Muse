package com.deameamo.muse.longman.analysis

import com.deameamo.util.ArrayList

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

import scala.util.parsing.combinator.Parsers

object RuleParser {
  def main(args: Array[String]) {
    val desc = "ALT:*give ({NNS}[OBL]) (=impression/sense/idea=)[P3SING,IA]->{VBP}"
    val rules = parse(desc)
//    println(rules)
    rules.foreach(println(_))
    println("done")
  }
  
  val parser = new RuleParser
  
  def parse(desc: String): MutableList[Rule] = {
    val tokens = new parser.lexical.Scanner(desc)
    val s = parser.phrase(parser.compoundRules)(tokens)
    s match {
      case parser.Success(compoundRules, _) => {
        val list = new MutableList[Rule]
        compoundRules.foreach(compoundRule => list ++= compoundRule.rules)
        list
      }
      case parser.NoSuccess(msg, _) => new MutableList[Rule]
    }
  }
  
  def parseOne(desc: String): Rule = {
    val tokens = new parser.lexical.Scanner(desc)
    val s = parser.phrase(parser.rule)(tokens)
    s match {
      case parser.Success(rule, _) => rule
      case parser.NoSuccess(msg, _) => null
    }
  }
}

class RuleParser extends Parsers with RuleTokens {
  type Elem = lexical.Token
  type Tokens = RuleTokens
  
  val lexical = new RuleLexical
  
  implicit def keyword(chars: String): Parser[String] = acceptIf(_.chars == chars)("`"+chars+"' expected but " + _ + " found") ^^ (_.chars)
  
  def number: Parser[Int] = elem("number", _.isInstanceOf[Number]) ^^ (n => Integer.parseInt(n.chars))
  
  def literal: Parser[String] = elem("string literal", _.isInstanceOf[Literal]) ^^ (_.chars)
  
  val isCore: Parser[Boolean] = ("*"?) ^^ {
    case None => false
    case Some(a) => true
  }
  
  val forms1: Parser[MutableList[String]] = literal ^^ (form => (new MutableList[String]) += form)
  
  val forms2: Parser[MutableList[String]] = "`" ~> rep1(literal) <~ "`" ^^ (list => (new MutableList[String]) += list.mkString(" "))
  
  val forms3: Parser[MutableList[String]] = "(=" ~> rep1sep(literal, "/") <~ "=)" ^^ (list => (new MutableList[String]) ++= list)
  
  val forms4: Parser[MutableList[String]] = "(*" ~> rep1sep(literal, "/") <~ "*)" ^^ (list => (new MutableList[String]) ++= list)
  
  val forms: Parser[MutableList[String]] = (forms1 | forms2 | forms3 | forms4)
  
  val typo: Parser[String] = "{" ~> literal <~ "}"
  
  val values: Parser[ArrayList[String]] = (("[" ~> rep1sep(literal, ",") <~ "]")?) ^^ {
    case None => new ArrayList[String]
    case Some(list) => (new ArrayList[String]) ++= list
  }
  
  val expectation: Parser[Spec] = (("<" ~> spec <~ ">")?) ^^ {
    case None => null
    case Some(spec) => spec
  }
  
  val level: Parser[Int] = (("#" ~> number)?) ^^ {
    case None => 0
    case Some(level) => level
  }
  
  val id: Parser[String] = (("$" ~> literal)?) ^^ {
    case None => null
    case Some(id) => id
  }
  
  val operationAssign3: Parser[Operation] = literal ~ ("." ~> literal) ~ ("@" ~> literal) ^^ {
    case operandA~operandB~operandC => {
      val operation = new Operation(Operator.ASSIGN)
      operation.operands += operandA
      operation.operands += operandB
      operation.operands += operandC
      operation
    }
  }
  
  val operationSet3: Parser[Operation] = literal ~ ("." ~> literal) ~ ("=" ~> literal) ^^ {
    case operandA~operandB~operandC => {
      val operation = new Operation(Operator.SET)
      operation.operands += operandA
      operation.operands += operandB
      operation.operands += operandC
      operation
    }
  }
  
  val operationAdd3: Parser[Operation] = literal ~ ("." ~> literal) ~ ("+" ~> literal) ^^ {
    case operandA~operandB~operandC => {
      val operation = new Operation(Operator.ADD)
      operation.operands += operandA
      operation.operands += operandB
      operation.operands += operandC
      operation
    }
  }
  
  val operationAssign2: Parser[Operation] = literal ~ ("@" ~> literal) ^^ {
    case operandA~operandB => {
      val operation = new Operation(Operator.ASSIGN)
      operation.operands += operandA
      operation.operands += operandB
      operation
    }
  }
  
  val operationSet2: Parser[Operation] = literal ~ ("=" ~> literal) ^^ {
    case operandA~operandB => {
      val operation = new Operation(Operator.SET)
      operation.operands += operandA
      operation.operands += operandB
      operation
    }
  }
  
  val operationAdd2: Parser[Operation] = literal ~ ("+" ~> literal) ^^ {
    case operandA~operandB => {
      val operation = new Operation(Operator.ADD)
      operation.operands += operandA
      operation.operands += operandB
      operation
    }
  }
  
  val operationReplace2: Parser[Operation] = literal ~ ("." ~> literal) ^^ {
    case operandA~operandB => {
      val operation = new Operation(Operator.REPLACE)
      operation.operands += operandA
      operation.operands += operandB
      operation
    }
  }
  
  val operationCheck2: Parser[Operation] = literal ~ ("~" ~> literal) ^^ {
    case operandA~operandB => {
      val operation = new Operation(Operator.CHECK)
      operation.operands += operandA
      operation.operands += operandB
      operation
    }
  }
  
  val operationJoin2: Parser[Operation] = literal ~ ("%" ~> literal) ^^ {
    case operandA~operandB => {
      val operation = new Operation(Operator.JOIN)
      operation.operands += operandA
      operation.operands += operandB
      operation
    }
  }
  
  val operationInsert2: Parser[Operation] = literal ~ (">" ~> literal) ^^ {
    case operandA~operandB => {
      val operation = new Operation(Operator.INSERT)
      operation.operands += operandA
      operation.operands += operandB
      operation
    }
  }
  
  val operationInsert1: Parser[Operation] = literal <~ ">" ^^ {
    case operandA => {
      val operation = new Operation(Operator.INSERT)
      operation.operands += operandA
      operation
    }
  }
  
  val operationUpgrade1: Parser[Operation] = literal <~ ">" ^^ {
    case operandA => {
      val operation = new Operation(Operator.INSERT)
      operation.operands += operandA
      operation
    }
  }
  
  val operation1: Parser[Operation] = literal ^^ (operator => new Operation(operator))
  
  val operation = 
    operationAssign3 | operationSet3 | operationAdd3 |
    operationAssign2 | operationSet2 | operationAdd2 | operationReplace2 | operationCheck2 | operationJoin2 | operationInsert2 |
    operationInsert1 | operation1
  
  val operations: Parser[MutableList[Operation]] = (("^" ~> rep1sep(operation, ","))?) ^^ {
    case None => new MutableList[Operation]
    case Some(list) => (new MutableList[Operation]) ++= list
  }
  
//  val spec1: Parser[Spec] = isCore~forms~typo~values~expectation~level~id~operations ^^ {
//    case isCore~forms~typo~values~expectation~level~id~operations => new Spec(isCore, forms, typo, values, expectation, level, id, operations)
//    }
  
  val spec1: Parser[Spec] = isCore~forms~values~expectation~level~id~operations ^^ {
    case isCore~forms~values~expectation~level~id~operations => new Spec(isCore, forms, Head.UNDEF, values, expectation, level, id, operations)
    }
  
  val spec2: Parser[Spec] = isCore~typo~values~expectation~level~id~operations ^^ {
    case isCore~typo~values~expectation~level~id~operations => new Spec(isCore, new MutableList[String], typo, values, expectation, level, id, operations)
    }
  
  val spec = spec1 | spec2
  
  val rule = (literal <~ ":") ~ (rep1(spec) <~ "->") ~ spec ^^ {
    case category~body~product => new Rule(category, (new MutableList[Spec]) ++= body, product)
  }
  
  val optionalSpec: Parser[OptionalSpec] = "(" ~> rep1(spec) <~")" ^^ {specs => new OptionalSpec(specs)}
  
  val switchableSpec: Parser[SwitchableSpec] = "(~" ~> spec ~ spec <~ "~)" ^^ {
    case specA~specB => new SwitchableSpec(specA, specB)
  }
  
  val compoundSpec = spec | optionalSpec | switchableSpec
  
  val compoundRule = (literal <~ ":") ~ (rep1(compoundSpec) <~ "->") ~ spec ^^ {
    case category~body~product => new CompoundRule(category, body, product)
  }
  
  val compoundRules = rep1sep(compoundRule, "|")
}
