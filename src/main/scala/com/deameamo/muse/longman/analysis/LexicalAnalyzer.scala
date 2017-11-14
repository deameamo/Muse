package com.deameamo.muse.longman.analysis

import scala.collection.mutable

object LexicalAnalyzer {

  def main(args: Array[String]) {
    var sentence = "I have er to do this"
    val list = analyze(sentence)
    list.foreach(lexical => lexical.printMorph())
    println("done")
  }

  def analyze(sentence: String): mutable.MutableList[Lexical] = {
    val list = new mutable.MutableList[Lexical]
    val words = sentence.split(" ")
    words.foreach(word => list += new WordLexical(word))
    list
  }
}