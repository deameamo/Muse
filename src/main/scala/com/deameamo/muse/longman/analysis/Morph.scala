package com.deameamo.muse.longman.analysis

import scala.collection.mutable

abstract class Morph(val form: String, val pos: String, val entry: Entry, var originality: String) {
  def createPhrases(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase]
  
  def createPhrasesForUnits(units: mutable.HashSet[EntryUnit], bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    null
  }
  
  def getHwd = entry.hwd
  
  def equalsTo(another: Morph): Boolean = this.form == another.form && this.pos == another.pos && this.entry == another.entry && this.originality == another.originality
  
  override def toString = s"${entry.hwd}(${form})[${pos},${originality}][${entry.name}]"
}

class NounMorph(form: String, entry: Entry, val refPersonNumber: String, originality: String)
    extends Morph(form, POS.NOUN, entry, originality) {
  val role = AspectValue.REG
  var genre = AspectValue.SBSTH
  
  def createPhrases(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val entryConfig = HeadConfigAnalyzer.getEntryNNPConfig(form, entry, refPersonNumber, originality)
//    println(s"  ${entryConfig}")
    val entryGrams = entry.grams(entry.hwd)
    entry.senses.foreach(sense => {
      val senseGrams = sense.grams(sense.hwd)
      val grams = new mutable.MutableList[Gram]
      grams ++= senseGrams
      grams ++= entryGrams
      val senseConfig = 
        if(sense.ruleDesc == null)
          HeadConfigAnalyzer.getUnitNNPConfig(form, sense.hwd, sense, entryConfig)
        else
          null
      if(sense.subsenses.size == 0) {
        list ++= createForUnit(false, sense.hwd, sense.ruleDesc, sense, entryConfig, senseConfig, grams, bagOfHwds)
        sense.variants.foreach(variant => {
          if(variant.ruleDesc != null) {
            list ++= createForUnit(false, variant.hwd, variant.ruleDesc, sense, entryConfig, null, grams, bagOfHwds)
          }
          else {
            if(variant.hwd == form) {
              val variantConfig = HeadConfigAnalyzer.getUnitNNPConfig(form, variant.hwd, sense, entryConfig)
              list ++= createForUnit(false, variant.hwd, variant.ruleDesc, sense, entryConfig, variantConfig, grams, bagOfHwds)
            }
          }
        })
        sense.exas.foreach(exa => {
          list ++= createForUnit(false, exa.hwd, exa.ruleDesc, exa, entryConfig, senseConfig, grams, bagOfHwds)
        })
      }
      sense.subsenses.foreach(subsense => {
        val grams = new mutable.MutableList[Gram]
        grams ++= senseGrams
        grams ++= entryGrams
        grams ++= subsense.grams(subsense.hwd)
        list ++= createForUnit(false, subsense.hwd, subsense.ruleDesc, subsense, entryConfig, senseConfig, grams, bagOfHwds)
        subsense.variants.foreach(variant => {
          if(variant.ruleDesc != null) {
            list ++= createForUnit(false, variant.hwd, variant.ruleDesc, subsense, entryConfig, null, grams, bagOfHwds)
          }
          else {
            if(variant.hwd == form) {
              val variantConfig = HeadConfigAnalyzer.getUnitNNPConfig(form, variant.hwd, sense, entryConfig)
              list ++= createForUnit(false, variant.hwd, variant.ruleDesc, subsense, entryConfig, variantConfig, grams, bagOfHwds)
            }
          }
        })
        subsense.exas.foreach(exa => {
          list ++= createForUnit(false, exa.hwd, exa.ruleDesc, exa, entryConfig, senseConfig, grams, bagOfHwds)
        })
      })
    })
    list
  }
  
  override def createPhrasesForUnits(units: mutable.HashSet[EntryUnit], bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val entryConfig = HeadConfigAnalyzer.getEntryNNPConfig(form, entry, refPersonNumber, originality)
    val entryGrams = entry.grams(entry.hwd)
    units.foreach(unit => {
      val grams = new mutable.MutableList[Gram]
      grams ++= entryGrams
      if(unit.typo == EntryUnit.SENSE) {
        grams ++= unit.grams(unit.hwd)
      }
      else {
        val sense = unit.asInstanceOf[Subsense].parent
        grams ++= sense.grams(unit.hwd)
        grams ++= unit.grams(unit.hwd)
      }
      list ++= createForUnit(true, form, null, unit, entryConfig, null, grams, bagOfHwds)
    })
    list
  }
  
  private def createForUnit(fromMWE: Boolean, hwd: String, ruleDesc: String, unit: EntryUnit, entryConfig: NNPConfig, unitConfig: NNPConfig, grams: mutable.MutableList[Gram], bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    // helpers
    def buildLexicalRuleDesc: String = {
      var desc = s"LEX:*`${hwd}`->{NNP}"
      grams.foreach(gram => {
        gram.key match {
          case Grams.BEF => {
            if(gram.degree == Grams.ALWAYS) {
              val values = gram.value.split("/")
              val befDescs = new mutable.MutableList[String]
              values.foreach(value => {
                befDescs += s"LEX:*`${hwd}`$$A ${value}[PPP]$$B^A.POSTMOD+B->{NNP}"
              })
              desc = s"${desc}|${befDescs.mkString("|")}"
            }
          }
          case _ =>
        }
      })
      desc
    }
    
//    println(s"${unit.index}: hwd=${hwd}, ruleDesc=${ruleDesc}, unitConfig=${unitConfig}, ${form}")
    
    if(fromMWE == false) {
      if(ruleDesc != null) {
        val nnpHead = new NNPHead(entryConfig.form, entryConfig.originality)
        nnpHead.init(entryConfig, AspectValue.UNDEF)
        RuleParser.parse(ruleDesc).foreach(rule => {
          val phrase = PhraseFactory.create(rule)
          if(phrase.typo == Phrase.MWE) {
            if(phrase.asInstanceOf[MWEPhrase].init(hwd, entry, unit, nnpHead, bagOfHwds))
              list += phrase
          }
          else {
            if(phrase.init(unit, nnpHead, bagOfHwds)) {
              if(phrase.typo == Phrase.UPGRADER)
                PhraseFactory.upgraders += phrase
              else
                list += phrase
            }
          }
        })
      }
      else {
        if(unitConfig != null) {
          val ruleDesc = buildLexicalRuleDesc
          unitConfig.countability.foreach(countability => {
            val nnpHead = new NNPHead(hwd, unitConfig.originality)
            nnpHead.init(unitConfig, countability)
            RuleParser.parse(ruleDesc).foreach(rule => {
  //            println(s"NounMorph.createForUnit: rule=${rule}, ruleDesc=${ruleDesc}")
              val phrase = PhraseFactory.create(rule)
              if(phrase.init(unit, nnpHead, bagOfHwds))
                list += phrase
            })
          })
        }
      }
    }
    else {
      val ruleDesc = buildLexicalRuleDesc
      val nnpHead = new NNPHead(hwd, entryConfig.originality)
      nnpHead.init(entryConfig, AspectValue.UNDEF)
      RuleParser.parse(ruleDesc).foreach(rule => {
        val phrase = PhraseFactory.create(rule)
        if(phrase.init(unit, nnpHead, bagOfHwds))
          list += phrase
      })
    }
    list
  }
  
  override def equalsTo(another: Morph) = {
    if(super.equalsTo(another)) {
      val anotherNounMorph = another.asInstanceOf[NounMorph]
      this.refPersonNumber == anotherNounMorph.refPersonNumber
    }
    else
      false
  }
  
  override def toString = s"${entry.hwd}(${form})[${pos},${role},${refPersonNumber},${genre},${originality}][${entry.name}]"
}

class PronounMorph(form: String, entry: Entry, originality: String) 
    extends Morph(form, POS.PRONOUN, entry, originality){
  var role = AspectValue.UNDEF
  var genre = AspectValue.SBSTH
  var personNumber = AspectValue.UNDEF
  var qw = AspectValue.PLAIN
  val grams = entry.grams(form)
  grams.foreach(gram => {
    if(gram.key == Grams.ASP) {
      if(AspectValue.ROLES.contains(gram.value))
        role = gram.value
      else if(AspectValue.GENRES.contains(gram.value))
        genre = gram.value
      else if(AspectValue.PERSON_NUMBERS.contains(gram.value))
        personNumber = gram.value
    }
    if(gram.key == Grams.QWT) {
      qw = gram.value
    }
  })
  
  def createPhrases(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    entry.senses.foreach(sense => {
      if(sense.subsenses.size == 0) {
        list ++= createForUnit(false, sense, originality, bagOfHwds)
        sense.exas.foreach(exa => {
          list ++= createForUnit(false, exa, originality, bagOfHwds)
        })
      }
      sense.subsenses.foreach(subsense => {
        list ++= createForUnit(false, subsense, originality, bagOfHwds)
        subsense.exas.foreach(exa => {
          list ++= createForUnit(false, exa, originality, bagOfHwds)
        })
      })
    })
    list
  }
  
  private def createForUnit(fromMWE: Boolean, unit: EntryUnit, originality: String, bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val specDesc = {
      if(unit.ruleDesc != null)
        unit.ruleDesc
      else
        s"LEX:*`${unit.hwd}`->{PN}"
    }
    val head = new PNHead(entry.hwd, originality)
    head.init(role, genre, personNumber, qw)
    RuleParser.parse(specDesc).foreach(rule => {
      val phrase = PhraseFactory.create(rule)
      if(phrase.init(unit, head, bagOfHwds))
        list += phrase
    })
    list
  }
  
  override def equalsTo(another: Morph) = {
    if(super.equalsTo(another)) {
      val peer = another.asInstanceOf[PronounMorph]
      this.role == peer.role && this.personNumber == peer.personNumber && this.genre == peer.genre
    }
    else
      false
  }
  
  override def toString = s"${entry.hwd}(${form})[${pos},${role},${genre},${personNumber},${originality}][${entry.name}]"
}

class VerbMorph(form: String, entry: Entry, val category: String, val isStem: String, val function: String, val tense: String, val personNumber: String, originality: String) 
    extends Morph(form, POS.VERB, entry, originality){
  
  def createPhrases(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    category match {
      case POS.VERB => createVBPPhrase(bagOfHwds)
      case POS.MODAL => createVBMPhrase(bagOfHwds)
      case POS.AUX => createVBAPhrase(bagOfHwds)
      case POS.PRESPART => new mutable.MutableList[Phrase]
    }
  }
  
  private def createVBPPhrase(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val entryConfig = HeadConfigAnalyzer.getEntryVBPConfig(form, entry, isStem, function, tense, personNumber, originality)
//    println(s"  entryConfig=${entryConfig}, entry=${entry.name}")
    val entryGrams = entry.grams(entry.hwd)
    entry.senses.foreach(sense => {
      val senseGrams = sense.grams(sense.hwd)
      val grams = new mutable.MutableList[Gram]
      grams ++= senseGrams
      grams ++= entryGrams
      val senseConfig = 
        if(sense.ruleDesc == null) 
          HeadConfigAnalyzer.getUnitVBPConfig(form, sense.hwd, sense, entryConfig)
        else
          null
      if(sense.subsenses.size == 0) {
        list ++= createVBPPhraseForUnit(false, sense.hwd, sense.ruleDesc, sense, entryConfig, senseConfig, grams, bagOfHwds)
        sense.variants.foreach(variant => {
          if(variant.ruleDesc != null) {
            list ++= createVBPPhraseForUnit(false, variant.hwd, variant.ruleDesc, sense, entryConfig, null, grams, bagOfHwds)
          }
          else {
            if(variant.hwd == form) {
              val variantConfig = HeadConfigAnalyzer.getUnitVBPConfig(form, variant.hwd, sense, entryConfig)
              list ++= createVBPPhraseForUnit(false, variant.hwd, variant.ruleDesc, sense, entryConfig, variantConfig, grams, bagOfHwds)
            }
          }
        })
        sense.exas.foreach(exa => {
          list ++= createVBPPhraseForUnit(false, exa.hwd, exa.ruleDesc, exa, entryConfig, senseConfig, grams, bagOfHwds)
        })
      }
      sense.subsenses.foreach(subsense => {
        val grams = new mutable.MutableList[Gram]
        grams ++= senseGrams
        grams ++= entryGrams
        grams ++= subsense.grams(subsense.hwd)
        list ++= createVBPPhraseForUnit(false, subsense.hwd, subsense.ruleDesc, subsense, entryConfig, senseConfig, grams, bagOfHwds)
        subsense.variants.foreach(variant => {
          if(variant.ruleDesc != null) {
            list ++= createVBPPhraseForUnit(false, variant.hwd, variant.ruleDesc, subsense, entryConfig, null, grams, bagOfHwds)
          }
          else {
            if(variant.hwd == form) {
              val variantConfig = HeadConfigAnalyzer.getUnitVBPConfig(form, variant.hwd, sense, entryConfig)
              list ++= createVBPPhraseForUnit(false, variant.hwd, variant.ruleDesc, subsense, entryConfig, variantConfig, grams, bagOfHwds)
            }
          }
        })
        subsense.exas.foreach(exa => {
          list ++= createVBPPhraseForUnit(false, exa.hwd, exa.ruleDesc, exa, entryConfig, senseConfig, grams, bagOfHwds)
        })
      })
    })
    list
  }
  
  private def createVBMPhrase(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    entry.senses.foreach(sense => {
      if(sense.subsenses.size == 0) {
        list ++= createVBMPhraseForUnit(sense.ruleDesc, sense, originality, bagOfHwds)
        sense.exas.foreach(exa => {
          list ++= createVBMPhraseForUnit(exa.ruleDesc, exa, originality, bagOfHwds)
        })
      }
      sense.subsenses.foreach(subsense => {
        list ++= createVBMPhraseForUnit(subsense.ruleDesc, subsense, originality, bagOfHwds)
        subsense.exas.foreach(exa => {
          list ++= createVBMPhraseForUnit(exa.ruleDesc, exa, originality, bagOfHwds)
        })
      })
    })
    list
  }
  
  private def createVBAPhrase(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    entry.senses.foreach(sense => {
      if(sense.subsenses.size == 0) {
        list ++= createVBAPhraseForUnit(sense.ruleDesc, sense, originality, bagOfHwds)
        sense.exas.foreach(exa => {
          list ++= createVBAPhraseForUnit(exa.ruleDesc, sense, originality, bagOfHwds)
        })
      }
      sense.subsenses.foreach(subsense => {
        list ++= createVBAPhraseForUnit(subsense.ruleDesc, subsense, originality, bagOfHwds)
        subsense.exas.foreach(exa => {
          list ++= createVBAPhraseForUnit(exa.ruleDesc, subsense, originality, bagOfHwds)
        })
      })
    })
    list
  }
  
  override def createPhrasesForUnits(units: mutable.HashSet[EntryUnit], bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    category match {
      case POS.VERB => createVBPPhraseForUnits(units, bagOfHwds)
      case POS.MODAL => createVBMPhraseForUnits(units, bagOfHwds)
      case POS.AUX => new mutable.MutableList[Phrase]
      case POS.PRESPART => new mutable.MutableList[Phrase]
    }
  }
  
  private def createVBPPhraseForUnits(units: mutable.HashSet[EntryUnit], bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val entryConfig = HeadConfigAnalyzer.getEntryVBPConfig(form, entry, isStem, function, tense, personNumber, originality)
    val entryGrams = entry.grams(entry.hwd)
    units.foreach(unit => {
      val grams = new mutable.MutableList[Gram]
      grams ++= entryGrams
      if(unit.typo == EntryUnit.SENSE) {
        grams ++= unit.grams(unit.hwd)
      }
      else {
        val sense = unit.asInstanceOf[Subsense].parent
        grams ++= sense.grams(unit.hwd)
        grams ++= unit.grams(unit.hwd)
      }
      list ++= createVBPPhraseForUnit(true, null, null, unit, entryConfig, null, grams, bagOfHwds)
    })
    list
  }
  
  private def createVBMPhraseForUnits(units: mutable.HashSet[EntryUnit], bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    units.foreach(unit => {
      list ++= createVBMPhraseForUnit(form, unit, originality, bagOfHwds)
    })
    list
  }
  
  private def createVBPPhraseForUnit(fromMWE: Boolean, hwd: String, ruleDesc: String, unit: EntryUnit, entryConfig: VBPConfig, unitConfig: VBPConfig, grams: mutable.MutableList[Gram], bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    def buildLexicalRuleDesc(transitivity: String): String = {
      var desc = {
        transitivity match {
          case AspectValue.TRANS => s"LEX:*`${hwd}` {NNS}[OBL]$$A->{VBP}^VAR+A"
          case AspectValue.INTRANS => s"LEX:*`${hwd}`->{VBP}"
        }
      }
      grams.foreach(gram => {
        gram.key match {
          case _ =>
        }
      })
      desc
    }
    val list = new mutable.MutableList[Phrase]
//    println(s"${unit.index}: ${unit.lexUnit}, ${unit.grams(unit.hwd)}, ${senseConfig}, ${form}")
    
    if(fromMWE == false) {
      if(ruleDesc != null) {
        val vbpHead = new VBPHead(entryConfig.form, entryConfig.originality)
        vbpHead.init(entryConfig, AspectValue.UNDEF)
        RuleParser.parse(ruleDesc).foreach(rule => {
          val phrase = PhraseFactory.create(rule)
          if(phrase.typo == Phrase.MWE) {
            if(phrase.asInstanceOf[MWEPhrase].init(hwd, entry, unit, vbpHead, bagOfHwds))
              list += phrase
          }
          else {
            if(phrase.init(unit, vbpHead, bagOfHwds)) {
              if(phrase.typo == Phrase.UPGRADER)
                PhraseFactory.upgraders += phrase
              else
                list += phrase
            }
          }
        })
      }
      else {
        if(unitConfig != null) {
          unitConfig.transitivity.foreach(transitivity => {
            val vbpHead = new VBPHead(hwd, unitConfig.originality)
            val specDesc = buildLexicalRuleDesc(transitivity)
            vbpHead.init(unitConfig, transitivity)
            RuleParser.parse(specDesc).foreach(rule => {
              val phrase = PhraseFactory.create(rule)
              if(phrase.init(unit, vbpHead, bagOfHwds))
                list += phrase
            })
          })
        }
      }
    }
    else {
      entryConfig.transitivity.foreach(transitivity => {
        val vbpHead = new VBPHead(hwd, unitConfig.originality)
        val specDesc = buildLexicalRuleDesc(transitivity)
        vbpHead.init(unitConfig, transitivity)
        RuleParser.parse(specDesc).foreach(rule => {
          val phrase = PhraseFactory.create(rule)
          if(phrase.init(unit, vbpHead, bagOfHwds))
            list += phrase
        })
      })
    }
    list
  }
  
  private def createVBMPhraseForUnit(ruleDesc: String, unit: EntryUnit, originality: String, bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val head = new VBMHead(entry.hwd, originality)
    if(ruleDesc != null) {
      RuleParser.parse(ruleDesc).foreach(rule => {
        val phrase = PhraseFactory.create(rule)
        if(phrase.init(unit, head, bagOfHwds))
          list += phrase
      })
    }
    list
  }
  
  private def createVBAPhraseForUnit(ruleDesc: String, unit: EntryUnit, originality: String, bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val head = new VBAHead(entry.hwd, originality)
    head.init(isStem, function, tense, personNumber)
    if(ruleDesc != null) {
      RuleParser.parse(ruleDesc).foreach(rule => {
        val phrase = PhraseFactory.create(rule)
        if(phrase.init(unit, head, bagOfHwds))
          list += phrase
      })
    }
    list
  }
  
  override def equalsTo(another: Morph) = {
    if(super.equalsTo(another)) {
      val peer = another.asInstanceOf[VerbMorph]
      this.category == peer.category && this.isStem == peer.isStem && this.function == peer.function && this.tense == peer.tense && this.personNumber == peer.personNumber
    }
    else
      false
  }
  
  override def toString = s"${entry.hwd}(${form})[${pos},${category},${isStem},${function},${tense},${personNumber},${originality}][${entry.name}]"
}

class AdjectiveMorph(form: String, entry: Entry, val degree: String, originality: String)
    extends Morph(form, POS.ADJECTIVE, entry, originality) {
  
  def createPhrases(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val entryGrams = entry.grams(entry.hwd)
    entry.senses.foreach(sense => {
      val senseGrams = sense.grams(sense.hwd)
      val grams = new mutable.MutableList[Gram]
      grams ++= senseGrams
      grams ++= entryGrams
      if(sense.subsenses.size == 0) {
        list ++= createForUnit(false, sense.hwd, sense.ruleDesc, sense, grams, bagOfHwds)
        sense.variants.foreach(variant => {
          list ++= createForUnit(false, variant.hwd, variant.ruleDesc, sense, grams, bagOfHwds)
        })
        sense.exas.foreach(exa => {
          list ++= createForUnit(false, exa.hwd, exa.ruleDesc, exa, grams, bagOfHwds)
        })
      }
      sense.subsenses.foreach(subsense => {
        val grams = new mutable.MutableList[Gram]
        grams ++= senseGrams
        grams ++= entryGrams
        grams ++= subsense.grams(subsense.hwd)
        list ++= createForUnit(false, subsense.hwd, subsense.ruleDesc, subsense, grams, bagOfHwds)
        subsense.variants.foreach(variant => {
          list ++= createForUnit(false, variant.hwd, variant.ruleDesc, subsense, grams, bagOfHwds)
        })
        subsense.exas.foreach(exa => {
          list ++= createForUnit(false, exa.hwd, exa.ruleDesc, exa, grams, bagOfHwds)
        })
      })
    })
    list
  }
  
  override def createPhrasesForUnits(units: mutable.HashSet[EntryUnit], bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val entryGrams = entry.grams(entry.hwd)
    units.foreach(unit => {
      val grams = new mutable.MutableList[Gram]
      grams ++= entryGrams
      if(unit.typo == EntryUnit.SENSE) {
        grams ++= unit.grams(unit.hwd)
      }
      else {
        val sense = unit.asInstanceOf[Subsense].parent
        grams ++= sense.grams(unit.hwd)
        grams ++= unit.grams(unit.hwd)
      }
      list ++= createForUnit(true, form, null, unit, grams, bagOfHwds)
    })
    list
  }
  
  private def createForUnit(fromMWE: Boolean, hwd: String, ruleDesc: String, unit: EntryUnit, grams: mutable.MutableList[Gram], bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    def buildLexicalRuleDesc: String = {
      var desc = s"LEX:*`${hwd}`->{AJE}|LEX:`${hwd}`$$A *{NNP}[NODT]->{NNP}^PREMOD+A"
      grams.foreach(gram => {
        gram.key match {
          case Grams.BEF => {
            gram.value match {
              case Grams.NOUN => {
                if(gram.degree == Grams.NOT)
                  desc = s"LEX:*`${hwd}`->{AJE}|LEX:`${hwd}` *{NNP}[NODT]->{ERROR}"
                else
//                  desc = s"LEX:*`${entry.hwd}`->{AJE}|LEX:`${entry.hwd}`$$A *{NNP}[NOPREMOD,NODT]->{NNP}^PREMOD+A"
                  desc = s"LEX:*`${hwd}`->{AJE}|LEX:`${hwd}`$$A *{NNP}[NODT]->{NNP}^PREMOD+A"
              }
              case _ =>
            }
          }
          case _ =>
        }
      })
      desc
    }
    
    val list = new mutable.MutableList[Phrase]
    val head = new AJEHead(entry.hwd, originality)
    head.init(degree)
    if(fromMWE == false) {
      if(ruleDesc != null) {
        RuleParser.parse(ruleDesc).foreach(rule => {
          val phrase = PhraseFactory.create(rule)
          if(phrase.isInstanceOf[MWEPhrase]) {
            if(phrase.asInstanceOf[MWEPhrase].init(hwd, entry, unit, head, bagOfHwds))
              list += phrase
          }
          else {
            if(phrase.init(unit, head, bagOfHwds))
              list += phrase
          }
        })
      }
      else {
        val ruleDesc = buildLexicalRuleDesc
        RuleParser.parse(ruleDesc).foreach(rule => {
          val phrase = PhraseFactory.create(rule)
          if(phrase.init(unit, head, bagOfHwds))
            list += phrase
        })
      }
    }
    else {
      val ruleDesc = buildLexicalRuleDesc
      RuleParser.parse(ruleDesc).foreach(rule => {
        val phrase = PhraseFactory.create(rule)
        if(phrase.init(unit, head, bagOfHwds))
          list += phrase
      })
    }
    list
  }
  
  override def equalsTo(another: Morph) = {
    if(super.equalsTo(another)) {
      val peer = another.asInstanceOf[AdjectiveMorph]
      this.degree == peer.degree
    }
    else
      false
  }
  
  override def toString = s"${entry.hwd}(${form})[${pos},${degree},${originality}][${entry.name}]"
}

class AdverbMorph(form: String, entry: Entry, val degree: String, originality: String)
    extends Morph(form, POS.ADVERB, entry, originality) {
  
  def createPhrases(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val entryGrams = entry.grams(entry.hwd)
    entry.senses.foreach(sense => {
      val senseGrams = sense.grams(sense.hwd)
      val grams = new mutable.MutableList[Gram]
      grams ++= senseGrams
      grams ++= entryGrams
      if(sense.subsenses.size == 0) {
        list ++= createForUnit(false, sense.hwd, sense.ruleDesc, sense, grams, bagOfHwds)
        sense.variants.foreach(variant => {
          list ++= createForUnit(false, variant.hwd, variant.ruleDesc, sense, grams, bagOfHwds)
        })
        sense.exas.foreach(exa => {
          list ++= createForUnit(false, exa.hwd, exa.ruleDesc, exa, grams, bagOfHwds)
        })
      }
      sense.subsenses.foreach(subsense => {
        val grams = new mutable.MutableList[Gram]
        grams ++= senseGrams
        grams ++= entryGrams
        grams ++= subsense.grams(subsense.hwd)
        list ++= createForUnit(false, subsense.hwd, subsense.ruleDesc, subsense, grams, bagOfHwds)
        subsense.variants.foreach(variant => {
          list ++= createForUnit(false, variant.hwd, variant.ruleDesc, subsense, grams, bagOfHwds)
        })
        subsense.exas.foreach(exa => {
          list ++= createForUnit(false, exa.hwd, exa.ruleDesc, exa, grams, bagOfHwds)
        })
      })
    })
    list
  }
  
  override def createPhrasesForUnits(units: mutable.HashSet[EntryUnit], bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val entryGrams = entry.grams(entry.hwd)
    units.foreach(unit => {
      val grams = new mutable.MutableList[Gram]
      grams ++= entryGrams
      if(unit.typo == EntryUnit.SENSE) {
        grams ++= unit.grams(unit.hwd)
      }
      else {
        val sense = unit.asInstanceOf[Subsense].parent
        grams ++= sense.grams(unit.hwd)
        grams ++= unit.grams(unit.hwd)
      }
      list ++= createForUnit(true, form, null, unit, grams, bagOfHwds)
    })
    list
  }
  
  private def createForUnit(fromMWE: Boolean, hwd: String, ruleDesc: String, unit: EntryUnit, grams: mutable.MutableList[Gram], bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {    
    def buildLexicalRuleDesc: String = {
      var desc = s"LEX:*`${hwd}`->{AVE}|LEX:`${hwd}`$$A *{AJE}->{AJE}^PREMOD+A|LEX:`${hwd}`$$A *{AVE}->{AVE}^PREMOD+A"
      grams.foreach(gram => {
        gram.key match {
          case Grams.BEF => {
            gram.value match {
              case Grams.ADJ_ADV => desc = s"LEX:*`${hwd}`->{AVE}|LEX:`${hwd}`$$A *{AJE}[NOPREMOD]->{AJE}^PREMOD+A|LEX:`${hwd}`$$A *{AVE}[NOPREMOD]->{AVE}^PREMOD+A"
              case Grams.ADJ => desc = s"LEX:*`${hwd}`->{AVE}|LEX:`${hwd}`$$A *{AJE}[NOPREMOD]->{AJE}^PREMOD+A"
              case _ =>
            }
          }
          case _ =>
        }
      })
      desc
    }
    
    val list = new mutable.MutableList[Phrase]
    val head = new AVEHead(entry.hwd, originality)
    head.init(degree)
    if(fromMWE == false) {
      if(ruleDesc != null) {
        RuleParser.parse(ruleDesc).foreach(rule => {
          val phrase = PhraseFactory.create(rule)
          if(phrase.isInstanceOf[MWEPhrase]) {
            if(phrase.asInstanceOf[MWEPhrase].init(hwd, entry, unit, head, bagOfHwds))
              list += phrase
          }
          else {
            if(phrase.init(unit, head, bagOfHwds))
              list += phrase
          }
        })
      }
      else {
        val ruleDesc = buildLexicalRuleDesc
        RuleParser.parse(ruleDesc).foreach(rule => {
          val phrase = PhraseFactory.create(rule)
          if(phrase.init(unit, head, bagOfHwds))
            list += phrase
        })
      }
    }
    else {
      val ruleDesc = buildLexicalRuleDesc
      RuleParser.parse(ruleDesc).foreach(rule => {
        val phrase = PhraseFactory.create(rule)
        if(phrase.init(unit, head, bagOfHwds))
          list += phrase
      })
    }
    list
  }
  
  override def equalsTo(another: Morph) = {
    if(super.equalsTo(another)) {
      val peer = another.asInstanceOf[AdverbMorph]
      this.degree == peer.degree
    }
    else
      false
  }
  
  override def toString = s"${entry.hwd}(${form})[${pos},${degree},${originality}][${entry.name}]"
}

class PrepositionMorph(form: String, entry: Entry, originality: String) 
    extends Morph(form, POS.PREPOSITION, entry, originality){
  def createPhrases(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    entry.senses.foreach(sense => {
      if(sense.subsenses.size == 0) {
        list ++= createForUnit(false, sense.hwd, sense.ruleDesc, sense, originality, bagOfHwds)
        sense.exas.foreach(exa => {
          list ++= createForUnit(false, exa.hwd, exa.ruleDesc, exa, originality, bagOfHwds)
        })
      }
      sense.subsenses.foreach(subsense => {
        list ++= createForUnit(false, subsense.hwd, subsense.ruleDesc, subsense, originality, bagOfHwds)
        subsense.exas.foreach(exa => {
          list ++= createForUnit(false, exa.hwd, exa.ruleDesc, exa, originality, bagOfHwds)
        })
      })
    })
    list
  }
  
  private def createForUnit(fromMWE: Boolean, hwd: String, ruleDesc: String, unit: EntryUnit, originality: String, bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    def buildLexicalRuleDesc: String = {
      s"LEX:*`${hwd}` {NNS}$$A->{PPP}^VAR+A"
    }
    
    val list = new mutable.MutableList[Phrase]
    val head = new PPPHead(entry.hwd, originality)
    if(fromMWE == false) {
      if(ruleDesc != null) {
        RuleParser.parse(ruleDesc).foreach(rule => {
          val phrase = PhraseFactory.create(rule)
          if(ruleDesc.startsWith("MWE")) {
            if(phrase.asInstanceOf[MWEPhrase].init(hwd, entry, unit, head, bagOfHwds))
              list += phrase
          }
          else {
            if(phrase.init(unit, head, bagOfHwds))
              list += phrase
          }
        })
      }
      else {
        val ruleDesc = buildLexicalRuleDesc
        RuleParser.parse(ruleDesc).foreach(rule => {
          val phrase = PhraseFactory.create(rule)
          if(phrase.init(unit, head, bagOfHwds))
            list += phrase
        })
      }
    }
    else {
      val ruleDesc = buildLexicalRuleDesc
      RuleParser.parse(ruleDesc).foreach(rule => {
        val phrase = PhraseFactory.create(rule)
        if(phrase.init(unit, head, bagOfHwds))
          list += phrase
      })
    }
    list
  }
}

class CommonMorph(form: String, pos: String, entry: Entry, originality: String)
    extends Morph(form, pos, entry, originality) {
  var qw = AspectValue.PLAIN
  val grams = entry.grams(form)
  grams.foreach(gram => {
    if(gram.key == Grams.QWT) {
      qw = gram.value
    }
  })
  def createPhrases(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    entry.senses.foreach(sense => {
      if(sense.subsenses.size == 0) {
        list ++= createForUnit(false, sense.hwd, sense.ruleDesc, sense, pos, originality, bagOfHwds)
        sense.exas.foreach(exa => {
          list ++= createForUnit(false, exa.hwd, exa.ruleDesc, exa, pos, originality, bagOfHwds)
        })
      }
      sense.subsenses.foreach(subsense => {
        list ++= createForUnit(false, subsense.hwd, subsense.ruleDesc, subsense, pos, originality, bagOfHwds)
        subsense.exas.foreach(exa => {
          list ++= createForUnit(false, exa.hwd, exa.ruleDesc, exa, pos, originality, bagOfHwds)
        })
      })
    })
    list
  }
  
  private def createForUnit(fromMWE: Boolean, hwd: String, ruleDesc: String, unit: EntryUnit, typo: String, originality: String, bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val head = new Head(entry.hwd, typo, originality)
    head.set(AspectName.QW_TYPE, qw)
    head.set(AspectName.QW_MARK, qw)
    if(ruleDesc != null) {
      RuleParser.parse(ruleDesc).foreach(rule => {
        val phrase = PhraseFactory.create(rule)
        if(phrase.init(unit, head, bagOfHwds))
          list += phrase
      })
    }
    list
  }
}

class MWEMorph(firstWord: String, entry: Entry, originality: String)
  extends Morph(firstWord, POS.MWE, entry, originality) {
  override def getHwd = firstWord
  
  def createPhrases(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    val list = new mutable.MutableList[Phrase]
    val head = new COMPHead(firstWord, originality)
    RuleParser.parse(entry.transRule).foreach(rule => {
      val phrase = PhraseFactory.create(rule).asInstanceOf[MWEPhrase]
      if(phrase.init(entry.hwd, entry, null, head, bagOfHwds))
        list += phrase
    })
    list
  }
}

class UnregisteredMorph(form: String) extends Morph(form, POS.UNREGISTERED, null, AspectValue.ORIG) {
  
  def createPhrases(bagOfHwds: mutable.HashSet[String]): mutable.MutableList[Phrase] = {
    new mutable.MutableList[Phrase]
  }
  
  override def toString = s"${form}[${pos}]"
}
