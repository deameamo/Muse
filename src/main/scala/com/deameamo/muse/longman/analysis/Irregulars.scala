package com.deameamo.muse.longman.analysis

import scala.collection.mutable

object Irregulars {

  private val map = new mutable.HashMap[String, mutable.MutableList[Morph]]

  def index(irregular: Irregular) {
    if (irregular.entry.hwd == "be") {
      addIntoMap("am", new VerbMorph("am", irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PRES, AspectValue.P1SING, AspectValue.ORIG))
      addIntoMap("are", new VerbMorph("are", irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PRES, AspectValue.P2, AspectValue.ORIG))
      addIntoMap("is", new VerbMorph("is", irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PRES, AspectValue.P3SING, AspectValue.ORIG))
      addIntoMap("are", new VerbMorph("are", irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PRES, AspectValue.P1PL, AspectValue.ORIG))
      addIntoMap("are", new VerbMorph("are", irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PRES, AspectValue.P3PL, AspectValue.ORIG))
      addIntoMap("was", new VerbMorph("was", irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.P1SING, AspectValue.ORIG))
      addIntoMap("were", new VerbMorph("were", irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.P2, AspectValue.ORIG))
      addIntoMap("was", new VerbMorph("was", irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.P3SING, AspectValue.ORIG))
      addIntoMap("were", new VerbMorph("were", irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.P1PL, AspectValue.ORIG))
      addIntoMap("were", new VerbMorph("were", irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.P3PL, AspectValue.ORIG))
      addIntoMap("been", new VerbMorph("been", irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PASTPART, AspectValue.UNDEF, AspectValue.UNDEF, AspectValue.ORIG))
      addIntoMap("be", new VerbMorph("be", irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.INFINITIVE, AspectValue.UNDEF, AspectValue.UNDEF, AspectValue.ORIG))
    }
    else {
      irregular.category match {
        case PLURAL => addIntoMap(irregular.form, new NounMorph(irregular.form, irregular.entry, AspectValue.P3PL, AspectValue.ORIG))
        case SINGULAR => addIntoMap(irregular.form, new NounMorph(irregular.form, irregular.entry, AspectValue.P3SING, AspectValue.ORIG))
        case P3SING => addIntoMap(irregular.form, new VerbMorph(irregular.form, irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PRES, AspectValue.P3SING, AspectValue.ORIG))
        case PAST => addIntoMap(irregular.form, new VerbMorph(irregular.form, irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.ALL, AspectValue.ORIG))
        case PRESPART => addIntoMap(irregular.form, new VerbMorph(irregular.form, irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PRESPART, AspectValue.UNDEF, AspectValue.UNDEF, AspectValue.ORIG))
        case PASTPART => addIntoMap(irregular.form, new VerbMorph(irregular.form, irregular.entry, irregular.entry.posList.head, AspectValue.INFL, AspectValue.PASTPART, AspectValue.UNDEF, AspectValue.UNDEF, AspectValue.ORIG))
        case COMP =>
          if (irregular.entry.isPos(POS.ADJECTIVE)) {
            addIntoMap(irregular.form, new AdjectiveMorph(irregular.form, irregular.entry, AspectValue.COMP, AspectValue.ORIG))
          }
          else if (irregular.entry.isPos(POS.ADVERB)) {
            addIntoMap(irregular.form, new AdverbMorph(irregular.form, irregular.entry, AspectValue.COMP, AspectValue.ORIG))
          }
        case SUPE =>
          if (irregular.entry.isPos(POS.ADJECTIVE)) {
            addIntoMap(irregular.form, new AdjectiveMorph(irregular.form, irregular.entry, AspectValue.SUPE, AspectValue.ORIG))
          }
          else if (irregular.entry.isPos(POS.ADVERB)) {
            addIntoMap(irregular.form, new AdverbMorph(irregular.form, irregular.entry, AspectValue.SUPE, AspectValue.ORIG))
          }
        case "negShortForm" =>

      }
    }
  }

  def getPluralForm(form: String, irregulars: mutable.MutableList[Irregular]): String = {
    if (irregulars.isEmpty)
      form
    else {
      val plurals = new mutable.MutableList[String]
      irregulars.foreach(irregular => {
        if (irregular.category == PLURAL) {
          plurals += irregular.form
        }
      })
      if (plurals.nonEmpty) {
        if (plurals.contains(form))
          form
        else
          plurals.mkString(",")
      }
      else
        form
    }
  }

  def getVerbForm(form: String, irregulars: mutable.MutableList[Irregular], hwd: String, function: String, tense: String, personNumber: String): String = {
    if (hwd == "be") {
      val correct = {
        if (function == AspectValue.PREDICATE && tense == AspectValue.PRES && personNumber == AspectValue.P1SING) "am"
        else if (function == AspectValue.PREDICATE && tense == AspectValue.PRES && personNumber == AspectValue.P2) "are"
        else if (function == AspectValue.PREDICATE && tense == AspectValue.PRES && personNumber == AspectValue.P3SING) "is"
        else if (function == AspectValue.PREDICATE && tense == AspectValue.PRES && personNumber == AspectValue.P1PL) "are"
        else if (function == AspectValue.PREDICATE && tense == AspectValue.PRES && personNumber == AspectValue.P3PL) "are"
        else if (function == AspectValue.PREDICATE && tense == AspectValue.PRES && personNumber == AspectValue.COMMON) "am,is,are"
        else if (function == AspectValue.PREDICATE && tense == AspectValue.PAST && personNumber == AspectValue.P1SING) "was"
        else if (function == AspectValue.PREDICATE && tense == AspectValue.PAST && personNumber == AspectValue.P2) "were"
        else if (function == AspectValue.PREDICATE && tense == AspectValue.PAST && personNumber == AspectValue.P3SING) "was"
        else if (function == AspectValue.PREDICATE && tense == AspectValue.PAST && personNumber == AspectValue.P1PL) "were"
        else if (function == AspectValue.PREDICATE && tense == AspectValue.PAST && personNumber == AspectValue.P3PL) "were"
        else if (function == AspectValue.PREDICATE && tense == AspectValue.PAST && personNumber == AspectValue.ALL) "was,were"
        else if (function == AspectValue.PASTPART && tense == AspectValue.UNDEF && personNumber == AspectValue.UNDEF) "been"
        else if (function == AspectValue.PRESPART && tense == AspectValue.UNDEF && personNumber == AspectValue.UNDEF) "being"
        else if (function == AspectValue.INFINITIVE && tense == AspectValue.UNDEF && personNumber == AspectValue.UNDEF) "be"
        else form
      }
      if (form == correct)
        form
      else
        correct
    }
    else {
      if (irregulars.isEmpty)
        form
      else {
        val corrects = new mutable.MutableList[String]
        irregulars.foreach(irregular => {
          if (irregular.category == P3SING && function == AspectValue.PREDICATE && tense == AspectValue.PRES && personNumber == AspectValue.P3SING) {
            corrects += irregular.form
          }
          else if (irregular.category == PAST && function == AspectValue.PREDICATE && tense == AspectValue.PAST && personNumber == AspectValue.ALL) {
            corrects += irregular.form
          }
          else if (irregular.category == PRESPART && function == AspectValue.PRESPART && tense == AspectValue.UNDEF && personNumber == AspectValue.UNDEF) {
            corrects += irregular.form
          }
          else if (irregular.category == PASTPART && function == AspectValue.PASTPART && tense == AspectValue.UNDEF && personNumber == AspectValue.UNDEF) {
            corrects += irregular.form
          }
        })
        if (corrects.nonEmpty) {
          if (corrects.contains(form))
            form
          else
            corrects.mkString(",")
        }
        else
          form
      }
    }
  }

  private def addIntoMap(form: String, morph: Morph) {
    if (!map.contains(form)) {
      map.put(form, new mutable.MutableList[Morph])
    }
    map.apply(form) += morph
  }

  def get(form: String, pos: String, originality: String): mutable.MutableList[Morph] = {
    val morphs = new mutable.MutableList[Morph]
    if (map.contains(form)) {
      map.apply(form).foreach(morph => {
        if (morph.pos == pos) {
          morph.originality = originality
          morphs += morph
        }
      })
    }
    morphs
  }

  val SINGULAR = "singularForm"
  val PLURAL = "pluralForm"
  val P3SING = "t3perssing"
  val PAST = "pastTense"
  val PRESPART = "presPart"
  val PASTPART = "pastPart"
  val COMP = "comp"
  val SUPE = "superl"
}

case class Irregular(form: String, category: String, entry: Entry)
