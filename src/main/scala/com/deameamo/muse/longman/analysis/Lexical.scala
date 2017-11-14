package com.deameamo.muse.longman.analysis

import scala.collection.mutable.HashSet
import scala.collection.mutable.MutableList

abstract class Lexical(val word: String) {
  
  val morphs = new MutableList[Morph]
  
  analyzeMorphs
  
  def addMorph(morph: Morph) {
    var target: Morph = null
    var i = 0
    while(i < morphs.size && target == null) {
      val iter = morphs.apply(i)
      if(iter.equalsTo(morph))
        target = iter
      i += 1
    }
    if(target == null)
      morphs += morph
  }
  
  def printMorph {
    println(s"${word}")
    morphs.foreach(morph => println(s"  ${morph}"))
  }
  
  def addMorphs(morphs: MutableList[Morph]) {
    morphs.foreach(addMorph(_))
  }
  
  def getHwds: HashSet[String]
  
  protected def analyzeMorphs
  
  def generateCell(bagOfHwds: HashSet[String]): Cell
  
}

class WordLexical(word: String) extends Lexical(word) {
  
  def analyzeMorphs {
    analyzeForm(word, AspectValue.ORIG)
    if(morphs.isEmpty && word.charAt(0).isUpper) {
      analyzeForm(word.toLowerCase, AspectValue.INORIG)
    }
    if(morphs.isEmpty) {
      addMorph(new UnregisteredMorph(word))
    }
//    printMorph
  }
  
  def getHwds: HashSet[String] = {
    val hwds = new HashSet[String]
    morphs.foreach(morph => {
      hwds += morph.getHwd
    })
    hwds
  }
  
  private def analyzeForm(form: String, originality: String) {
    analyzeAsNoun(form, originality)
    analyzeAsPronoun(form, originality)
    analyzeAsVerb(form, originality)
    analyzeAsAdjective(form, originality)
    analyzeAsAdverb(form, originality)
    analyzeAsPreposition(form, originality)
    analyzeAsCommon(form, originality)
//    analyzeAsMWE(form, originality)
  }
  
  private def analyzeAsNoun(form: String, originality: String) {
    addMorphs(Irregulars.get(form, POS.NOUN, originality))
    val aspects = reduceAsNoun(form)
    var reduced = false
    aspects.foreach(aspect => {
      Dictionary.lookupForm(aspect.stem, POS.NOUN).foreach(entry => {
        reduced = true
        addMorph(new NounMorph(form, entry, aspect.personNumber, originality))
      })
    })
    Dictionary.lookupForm(form, POS.NOUN).foreach(entry => {
      addMorph(new NounMorph(form, entry, if(reduced) AspectValue.P3PL else AspectValue.P3SING, originality))
    })
  }
  
  private def analyzeAsPronoun(form: String, originality: String) {
    Dictionary.lookupForm(form, POS.PRONOUN).foreach(entry => {
      addMorph(new PronounMorph(form, entry, originality))
    })
  }
  
  private def analyzeAsVerb(form: String, originality: String) {
    addMorphs(Irregulars.get(form, POS.VERB, originality))
    val aspects = reduceAsVerb(form)
    aspects.foreach(aspect => {
      Dictionary.lookupForm(aspect.stem, POS.VERB).foreach(entry => {
        addMorph(new VerbMorph(form, entry, entry.posList.head, aspect.isStem, aspect.function, aspect.tense, aspect.personNumber, originality))
      })
    })
    Dictionary.lookupForm(form, POS.VERB).foreach(entry => {
      // only for 'being'
      if(entry.posList.contains(POS.PRESPART)) {
        addMorph(new VerbMorph(form, entry, entry.posList.head, AspectValue.STEM, AspectValue.PRESPART, AspectValue.UNDEF, AspectValue.UNDEF, originality))
      }
      else {
        addMorph(new VerbMorph(form, entry, entry.posList.head, AspectValue.STEM, AspectValue.PREDICATE, AspectValue.PRES, AspectValue.COMMON, originality))
      }
    })
  }
  
  private def analyzeAsAdjective(form: String, originality: String) {
    addMorphs(Irregulars.get(form, POS.ADJECTIVE, originality))
    val aspects = reduceAsAdjective(form)
    aspects.foreach(aspect => {
      Dictionary.lookupForm(aspect.stem, POS.ADJECTIVE).foreach(entry => {
        addMorph(new AdjectiveMorph(form, entry, aspect.degree, originality))
      })
    })
    Dictionary.lookupForm(form, POS.ADJECTIVE).foreach(entry => {
      addMorph(new AdjectiveMorph(form, entry, AspectValue.POSI, originality))
    })
  }
  
  private def analyzeAsAdverb(form: String, originality: String) {
    addMorphs(Irregulars.get(form, POS.ADVERB, originality))
    val aspects = reduceAsAdjective(form)
    aspects.foreach(aspect => {
      Dictionary.lookupForm(aspect.stem, POS.ADVERB).foreach(entry => {
        addMorph(new AdverbMorph(form, entry, aspect.degree, originality))
      })
    })
    Dictionary.lookupForm(form, POS.ADVERB).foreach(entry => {
      addMorph(new AdverbMorph(form, entry, AspectValue.POSI, originality))
    })
  }
  
  private def analyzeAsPreposition(form: String, originality: String) {
    Dictionary.lookupForm(form, POS.PREPOSITION).foreach(entry => {
      addMorph(new PrepositionMorph(form, entry, originality))
    })
  }
  
  private def analyzeAsCommon(form: String, originality: String) {
    Dictionary.lookupForm(form).foreach(entry => {
      entry.posList.foreach(pos => {
        if(POS.isCommon(pos)) {
          addMorph(new CommonMorph(form, pos, entry, originality))
        }
      })
    })
  }
  
  private def analyzeAsMWE(firstWord: String, originality: String) {
    Dictionary.lookupMWE(firstWord).foreach(entry => {
      addMorph(new MWEMorph(firstWord, entry, originality))
    })
  }
  
  private def reduceAsNoun(implicit form: String): MutableList[NounAspects] = {
    val list = new MutableList[NounAspects]
    if(endsWith("ses") || endsWith("xes") || endsWith("zes") || endsWith("ches") || endsWith("shes")) {
      list += NounAspects(toStem(1), AspectValue.P3PL)
      list += NounAspects(toStem(2), AspectValue.P3PL)
    }
    else if(endsWith("ves")) {
      list += NounAspects(toStem(1), AspectValue.P3PL)
      list += NounAspects(toStem(3, "f"), AspectValue.P3PL)
      list += NounAspects(toStem(3, "fe"), AspectValue.P3PL)
    }
    else if(endsWith("oes")) {
      list += NounAspects(toStem(1), AspectValue.P3PL)
      list += NounAspects(toStem(2), AspectValue.P3PL)
    }
    else if(endsWith("ies")) {
      list += NounAspects(toStem(1), AspectValue.P3PL)
      list += NounAspects(toStem(3, "y"), AspectValue.P3PL)
    }
    else if(endsWith("i")) {
      list += NounAspects(toStem(1, "us"), AspectValue.P3PL)
    }
    else if(endsWith("es")) {
      list += NounAspects(toStem(1), AspectValue.P3PL)
      list += NounAspects(toStem(2, "is"), AspectValue.P3PL)
    }
    else if(endsWith("a")) {
      list += NounAspects(toStem(1, "um"), AspectValue.P3PL)
    }
    else if(endsWith("ae")) {
      list += NounAspects(toStem(2, "a"), AspectValue.P3PL)
    }
    else if(endsWith("s")) {
      list += NounAspects(toStem(1), AspectValue.P3PL)
    }
    list
  }
  
  def reduceAsVerb(implicit form: String): MutableList[VerbAspects] = {
    val list = new MutableList[VerbAspects]
    if(endsWith("ses") || endsWith("xes") || endsWith("zes") || endsWith("ches") || endsWith("shes") || endsWith("oes")) {
      list += VerbAspects(toStem(1), AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PRES, AspectValue.P3SING)
      list += VerbAspects(toStem(2), AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PRES, AspectValue.P3SING)
    }
    else if(endsWith("ies")) {
      list += VerbAspects(toStem(1), AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PRES, AspectValue.P3SING)
      list += VerbAspects(toStem(3, "y"), AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PRES, AspectValue.P3SING)
    }
    else if(endsWith("s")) {
      list += VerbAspects(toStem(1), AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PRES, AspectValue.P3SING)
    }
    else if(endsWith("ing")) {
      if(form.length > 6 && form.charAt(form.length - 4) == form.charAt(form.length - 5)) {
        list += VerbAspects(toStem(4), AspectValue.INFL, AspectValue.PRESPART, AspectValue.UNDEF, AspectValue.UNDEF)
        list += VerbAspects(toStem(3), AspectValue.INFL, AspectValue.PRESPART, AspectValue.UNDEF, AspectValue.UNDEF)
      }
      else {
        list += VerbAspects(toStem(3), AspectValue.INFL, AspectValue.PRESPART, AspectValue.UNDEF, AspectValue.UNDEF)
        list += VerbAspects(toStem(3, "e"), AspectValue.INFL, AspectValue.PRESPART, AspectValue.UNDEF, AspectValue.UNDEF)
      }
    }
    else if(endsWith("ed")) {
      if(endsWith("ied")) {
        list += VerbAspects(toStem(1), AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.ALL)
        list += VerbAspects(toStem(1), AspectValue.INFL, AspectValue.PASTPART, AspectValue.UNDEF, AspectValue.UNDEF)
        list += VerbAspects(toStem(2), AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.ALL)
        list += VerbAspects(toStem(2), AspectValue.INFL, AspectValue.PASTPART, AspectValue.UNDEF, AspectValue.UNDEF)
        list += VerbAspects(toStem(3, "y"), AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.ALL)
        list += VerbAspects(toStem(3, "y"), AspectValue.INFL, AspectValue.PASTPART, AspectValue.UNDEF, AspectValue.UNDEF)
      }
      else if(form.length > 5 && form.charAt(form.length - 3) == form.charAt(form.length - 4)) {
        list += VerbAspects(toStem(2), AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.ALL)
        list += VerbAspects(toStem(2), AspectValue.INFL, AspectValue.PASTPART, AspectValue.UNDEF, AspectValue.UNDEF)
        list += VerbAspects(toStem(3), AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.ALL)
        list += VerbAspects(toStem(3), AspectValue.INFL, AspectValue.PASTPART, AspectValue.UNDEF, AspectValue.UNDEF)
      }
      else {
        list += VerbAspects(toStem(1), AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.ALL)
        list += VerbAspects(toStem(1), AspectValue.INFL, AspectValue.PASTPART, AspectValue.UNDEF, AspectValue.UNDEF)
        list += VerbAspects(toStem(2), AspectValue.INFL, AspectValue.PREDICATE, AspectValue.PAST, AspectValue.ALL)
        list += VerbAspects(toStem(2), AspectValue.INFL, AspectValue.PASTPART, AspectValue.UNDEF, AspectValue.UNDEF)
      }
    }
    list
  }
  
  def reduceAsAdjective(implicit form: String): MutableList[AdjectiveAspects] = {
    val list = new MutableList[AdjectiveAspects]
    if(endsWith("er")) {
      if(endsWith("ier")) {
        list += AdjectiveAspects(toStem(1), AspectValue.COMP)
        list += AdjectiveAspects(toStem(2), AspectValue.COMP)
        list += AdjectiveAspects(toStem(3, "y"), AspectValue.COMP)
      }
      else if(form.length > 5 && form.charAt(form.length - 3) == form.charAt(form.length - 4)) {
        list += AdjectiveAspects(toStem(3), AspectValue.COMP)
      }
      else {
        list += AdjectiveAspects(toStem(1), AspectValue.COMP)
        list += AdjectiveAspects(toStem(2), AspectValue.COMP)
      }
    }
    else if(endsWith("est")) {
      if(endsWith("iest")) {
        list += AdjectiveAspects(toStem(2), AspectValue.SUPE)
        list += AdjectiveAspects(toStem(3), AspectValue.SUPE)
        list += AdjectiveAspects(toStem(4, "y"), AspectValue.SUPE)
      }
      else if(form.length > 6 && form.charAt(form.length - 4) == form.charAt(form.length - 5)) {
        list += AdjectiveAspects(toStem(4), AspectValue.SUPE)
      }
      else {
        list += AdjectiveAspects(toStem(2), AspectValue.SUPE)
        list += AdjectiveAspects(toStem(3), AspectValue.SUPE)
      }
    }
    list
  }
  
  private def endsWith(ending: String)(implicit form: String) = form.length > ending.length && form.endsWith(ending)
  
  private def toStem(endIndex: Int)(implicit form: String) = form.substring(0, form.length - endIndex)
  
  private def toStem(endIndex: Int, clitic: String)(implicit form: String) = form.substring(0, form.length - endIndex) + clitic
  
  case class NounAspects(stem: String, personNumber: String)
  
  case class VerbAspects(stem: String, isStem: String, function: String, tense: String, personNumber: String)
  
  case class AdjectiveAspects(stem: String, degree: String)
  
  def generateCell(bagOfHwds: HashSet[String]): Cell = {
    val cell = new Cell
    morphs.foreach(morph => {
      cell.addPhrases(morph.createPhrases(bagOfHwds))
    })
    cell.phrases.foreach(println(_))
    cell
  }
}