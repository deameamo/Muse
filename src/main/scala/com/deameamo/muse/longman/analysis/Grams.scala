package com.deameamo.muse.longman.analysis

import scala.collection.mutable.MutableList

object Grams {
  // degree
  val ALWAYS = 0
  val NOT = 1
  val USUALLY = 2
  val USUALLY_NOT = 3
  
  // key
  val BEF = "BEF"
  val AFT = "AFT"
  val CAT = "CAT"
  val INF = "INF"
  val ASP = "ASP"
  // question word type
  val QWT = "QWT"
  
  // CAT values
  val COUNTABLE = "countable"
  val UNCOUNTABLE = "uncountable"
  val TRANSITIVE = "transitive"
  val INTRANSITIVE = "intransitive"
  
  // INF values
  val SINGULAR = "singular"
  val PLURAL = "plural"
  val BIPOLAR = "bipolar"
  
  // BEF & AFT values
  val NOUN = "noun"
  val ADJ_ADV = "adjective/adverb"
  val ADJ = "adjective"
}

case class Gram(private val text: String) {
  var from = 0
  val degree = {
    if(text.startsWith("!")) {
      from = 2
      Grams.NOT
    }
    else if(text.startsWith("~!")) {
      from = 3
      Grams.USUALLY_NOT
    }
    else if(text.startsWith("~")) {
      from = 2
      Grams.USUALLY
    }
    else Grams.ALWAYS
  }
  val key = text.substring(from, from + 3)
  val value = text.substring(from + 4)
  
  override def toString = s"${degree} ${key} ${value}"
}
