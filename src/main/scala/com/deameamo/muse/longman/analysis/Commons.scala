package com.deameamo.muse.longman.analysis

import com.deameamo.commons.util.FileUtil

import scala.collection.mutable


object POS {
  def transfer(raws: Array[String]): mutable.MutableList[String] = {
    val transferals = new mutable.MutableList[String]
    raws.foreach(raw => {
      transferals += transfer(raw)
    })
    transferals
  }

  def transfer(raw: String): String = {
    raw match {
      case "noun" => POS.NOUN
      case "pronoun" => POS.PRONOUN
      case "number" => POS.NUMBER
      case "verb" => POS.VERB
      case "auxiliary verb" => POS.AUX
      case "modal verb" => POS.MODAL
      case "phrasal verb" => POS.PHRASAL_VERB
      case "adjective" => POS.ADJECTIVE
      case "adverb" => POS.ADVERB
      case "preposition" => POS.PREPOSITION
      case "conjunction" => POS.CONJUNCTION
      case "indefinite article" => POS.INDEFINITE
      case "definite article" => POS.DEFINITE
      case "determiner" => POS.DETERMINER
      case "predeterminer" => POS.PREDETERMINER
      case "interjection" => POS.INTERJECTION
      case "prefix" => POS.PREFIX
      case "suffix" => POS.SUFFIX
      case "last suffix" => POS.LAST_SUFFIX
      case "to" => POS.TO
      case "prespart" => POS.PRESPART
    }
  }

  def isCommon(pos: String): Boolean = COMMON.contains(pos)

  val NOUN = "NN"
  val PRONOUN = "PN"
  val NUMBER = "NM"
  val VERB = "VB"
  val AUX = "AU"
  val MODAL = "MO"
  val LINK = "LI"
  val PHRASAL_VERB = "PV"
  val ADJECTIVE = "AJ"
  val ADVERB = "AV"
  val PREPOSITION = "PP"
  val CONJUNCTION = "CJ"
  val INDEFINITE = "IA"
  val DEFINITE = "DA"
  val DETERMINER = "DT"
  val PREDETERMINER = "PD"
  val INTERJECTION = "IJ"
  val TO = "TO"
  val PREFIX = "PF"
  val SUFFIX = "SF"
  val LAST_SUFFIX = "LS"
  val PUNCTUATION = "PT"
  val PRESPART = "PR"
  val UNREGISTERED = "UR"
  val MWE = "MWE"

  val COMMON = Array(
    PREPOSITION,
    CONJUNCTION,
    INDEFINITE,
    DEFINITE,
    DETERMINER,
    PREDETERMINER,
    INTERJECTION,
    TO,
    NUMBER)
}

object Letter {
  val VOWEL = Seq('a', 'e', 'i', 'o', 'u')
  val VOWEL_Y = Seq('a', 'e', 'i', 'o', 'u', 'y')

  private val letterMap = new mutable.HashMap[Char, Char]
  private val list = FileUtil.readFile(Resource.PATH_LETTER_MAP)
  list.foreach(line => {
    letterMap.put(line.charAt(0), line.charAt(2))
    letterMap.put(line.charAt(0).toUpper, line.charAt(2).toUpper)
  })

  def isVowel(char: Char): Boolean = VOWEL.contains(char.toLower)

  def isVowelAndY(char: Char): Boolean = VOWEL_Y.contains(char.toLower)

  def containsVowel(word: String): Boolean = {
    var contains = false
    var i = 0
    while (i < word.length && !contains) {
      contains = VOWEL_Y.contains(word.charAt(i).toLower)
      i += 1
    }

    contains
  }

  def containsSpecialLetter(word: String): Boolean = {
    var does = false
    var i = 0
    while (i < word.length && !does) {
      does = letterMap.contains(word.charAt(i))
      i += 1
    }
    does
  }

  def transferToPlainWord(word: String): String = {
    var letters = new mutable.MutableList[Char]
    for (i <- 0 until word.length)
      letters += letterMap.getOrElse(word.charAt(i), word.charAt(i))
    letters.mkString
  }
}

object GEO {
  val AMERICAN = new GEO
  val BRITISH = new GEO
  val AUSTRALIAN = new GEO
  val DEFAULT = new GEO

  def getGEO(text: String): GEO = {
    text match {
      case "American English" => AMERICAN
      case "especially American English" => AMERICAN
      case "British English" => BRITISH
      case "especially British English" => BRITISH
      case "Australian English" => AUSTRALIAN
      case "especially Australian English" => AUSTRALIAN
      case "default" => DEFAULT
      case "" => DEFAULT
      case _ =>
        throw new Exception(s"Unknown geo: $text")
        null
    }
  }
}

class GEO