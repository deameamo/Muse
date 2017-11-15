package com.deameamo.muse.longman.analysis

import com.deameamo.commons.util.FileUtil

import scala.collection.mutable

object RuleEngine {

  val EQUI_RULES = "longman/analysis/ruleEngine/equiRules"
  val INSERTION_RULES = "longman/analysis/ruleEngine/insertionRules"
  val GENERAL_RULES = "longman/analysis/ruleEngine/generalRules"

  val equiRules = new mutable.MutableList[EquiRule]
  val generalRules = new mutable.MutableList[GeneralRule]
  val insertionRules = new mutable.MutableList[InsertionRule]

  FileUtil.readFile(EQUI_RULES).foreach(desc => {
    if (!desc.startsWith("//"))
      equiRules += new EquiRule(desc)
  })
  FileUtil.readFile(GENERAL_RULES).foreach(desc => {
    if (!desc.startsWith("//"))
      generalRules += new GeneralRule(desc)
  })
  FileUtil.readFile(INSERTION_RULES).foreach(desc => {
    if (!desc.startsWith("//"))
      insertionRules += new InsertionRule(desc)
  })

  def headConforms(headA: Head, headB: Head): Boolean = {
    if (headA.phrase.isCorePhrase != headB.phrase.isCorePhrase)
      return false
    if (headA.typo == headB.typo) {
      true
    }
    else {
      var conformed = false
      var i = 0
      var j = 0
      var specA: Spec = null
      var specB: Spec = null
      while (i < equiRules.size && !conformed) {
        specA = equiRules.apply(i).getEquiSpec(headA)
        j = 0
        while (j < equiRules.size && !conformed) {
          specB = equiRules.apply(j).getEquiSpec(headB)
          if (specA != null && specB != null)
            conformed = fitSpecs(specA, specB)
          j += 1
        }
        i += 1
      }
      conformed
    }
  }

  def getEquiHeadTypo(headA: Head, headB: Head): String = {
    var equiHeadTypo: String = null
    var i = 0
    var j = 0
    var specA: Spec = null
    var specB: Spec = null
    while (i < equiRules.size && equiHeadTypo == null) {
      specA = equiRules.apply(i).getEquiSpec(headA)
      j = 0
      while (j < equiRules.size && equiHeadTypo == null) {
        specB = equiRules.apply(j).getEquiSpec(headB)
        if (specA != null && specB != null && fitSpecs(specA, specB))
          equiHeadTypo = specA.typo
        j += 1
      }
      i += 1
    }
    equiHeadTypo
  }

  def fitSpecs(specA: Spec, specB: Spec): Boolean = {
    specA.typo == specB.typo
  }

  def fit(head: Head, spec: Spec): Boolean = {
    if (head.typo == Head.CJP) {
      if (spec.typo == Head.CJP) {
        performFitting(head, spec)
      }
      else {
        var fits = true
        var i = 0
        while (i < head.variables.size && fits) {
          fits = performFitting(head.variables.apply(i), spec)
          i += 1
        }
        fits
      }
    }
    else {
      performFitting(head, spec)
    }
  }

  def performFitting(head: Head, spec: Spec): Boolean = {
    def fitTypo: Boolean = {
      if (spec.typo == Head.ANY)
        return true
      if (head.typo == spec.typo) true
      else {
        var i = 0
        var fits = false
        while (i < equiRules.size && !fits) {
          fits = equiRules.apply(i).fit(head, spec)
          i += 1
        }
        fits
      }
    }

    if (spec == null)
      return false

    var fits = (if (spec.forms.nonEmpty) spec.forms.contains(head.form) else true) && (if (spec.typo != Head.UNDEF) fitTypo else true)
    //    if(head.typo == Head.CJP)
    //      println(s"fit (before): fits=${fits}, ${spec} vs ${head}")
    if (fits) {
      spec.stringAspects.foreach(pair => {
        fits = fits && isEquivalentValues(head.get(pair._1), pair._2)
      })
    }
    if (fits) {
      spec.headAspects.foreach(pair => {
        fits = fits && head.read(pair._1).typo == pair._2
      })
    }
    if (fits) {
      spec.listAspects.foreach(pair => {
        pair._1 match {
          case AspectName.PREMOD => fits = fits && head.preModifiers.size == pair._2
          case AspectName.POSTMOD => fits = fits && head.postModifiers.size == pair._2
        }
      })
    }
    //    if(head.typo == Head.CLAUSE)
    //      println(s"fit (after): fits=${fits}, ${spec} vs ${head}")
    fits
  }

  private def isEquivalentValues(headValue: String, specValue: String): Boolean = {
    specValue match {
      case AspectValue.NOM => headValue == AspectValue.NOM || headValue == AspectValue.REG
      case AspectValue.REG => headValue == AspectValue.NOM || headValue == AspectValue.OBL
      case AspectValue.OBL => headValue == AspectValue.OBL || headValue == AspectValue.REG
      case AspectValue.SBSTH => headValue == AspectValue.SBSTH || headValue == AspectValue.SB || headValue == AspectValue.STH
      case AspectValue.SB => headValue == AspectValue.SBSTH || headValue == AspectValue.SB
      case AspectValue.STH => headValue == AspectValue.SBSTH || headValue == AspectValue.STH
      case _ => headValue == specValue
    }
  }

  def canInsertByInsertionRules(insertee: Phrase, inserter: Head): InsertionRule = {
    var rule: InsertionRule = null
    var i = 0
    while (i < insertionRules.size && rule == null) {
      if (insertionRules.apply(i).canProduce(insertee, inserter))
        rule = insertionRules.apply(i)
      i += 1
    }
    rule
  }

  def absolveByGeneralRules(a: Phrase, b: Phrase): mutable.MutableList[Phrase] = {
    var productPhrases = new mutable.MutableList[Phrase]
    var i = 0
    while (i < generalRules.size) {
      val generalRule = generalRules.apply(i)
      val list = generalRule.produce(a, b)
      if (list != null)
        productPhrases ++= list
      i += 1
    }
    productPhrases
  }
}

class EquiRule(desc: String) {

  val rule: Rule = RuleParser.parseOne(desc)
  val equiA: Spec = rule.body.head
  val equiB: Spec = rule.product

  def fit(head: Head, spec: Spec): Boolean = {
    var equiAFits = head.typo == equiA.typo
    if (equiAFits) {
      equiA.stringAspects.foreach(pair => {
        equiAFits = equiAFits && head.get(pair._1) == pair._2
        //        println(s"equiRule.fit: name: ${pair._1}, value: ${pair._2}, peer: ${head.read(pair._1)}")
      })
    }
    val equiBFits = spec.typo == equiB.typo
    //    println(s"equiRule.fit: ${equiAFits}, ${equiA}, ${head}")
    equiAFits && equiBFits
  }

  def getEquiSpec(head: Head): Spec = {
    if (fit(head, equiB))
      equiB
    else
      null
  }
}

// Rule contains only two specs in body
class GeneralRule(desc: String) {

  val rule: Rule = RuleParser.parseOne(desc)
  rule.finish()
  val specA: Spec = rule.body.head
  val specB = rule.body.apply(1)

  def produce(a: Phrase, b: Phrase): mutable.MutableList[Phrase] = {
    if (a.coreHead != null && b.coreHead != null && RuleEngine.fit(a.coreHead, specA) && RuleEngine.fit(b.coreHead, specB)) {
      val aPeer = a.deepCopy
      val bPeer = b.deepCopy
      val corePhrase = if (specA.isCore) aPeer else bPeer
      val corePhraseHead = corePhrase.coreHead
      val productPhrase = rule.createProductPhrase(corePhrase)
      val map = new mutable.HashMap[String, Head]
      if (specA.id != null)
        map.put(specA.id, aPeer.coreHead)
      if (specB.id != null)
        map.put(specB.id, bPeer.coreHead)
      map.foreach(pair => {
        val spec = rule.getSpecById(pair._1)
        if (spec != null && spec.operations.nonEmpty) {
          pair._2.performOperations(spec.operations, map)
        }
      })
      if (productPhrase.typo == Phrase.STRUCTURER) {
        //        println(s"produce: rule=${rule}")
        //        val productPhrases = productPhrase.asInstanceOf[StructurerPhrase].performOperations(rule.product.operations, map)
        //        productPhrases.foreach(productPhrase => {
        //          productPhrase.coreHead.performOperations(rule.product.operations, map)
        //          productPhrase.coreHead.quality += Rule.getPoint(rule)
        //        })
        //        productPhrases

        productPhrase.coreHead.performOperations(rule.product.operations, map)
        productPhrase.coreHead.quality += Rule.getPoint(rule)
        productPhrase.asInstanceOf[StructurerPhrase].performOperations(rule.product.operations, map)
      }
      else {
        new mutable.MutableList[Phrase] += productPhrase
      }
    }
    else
      null
  }
}

class InsertionRule(desc: String) {
  val rule: Rule = RuleParser.parseOne(desc)
  rule.finish()
  val specA: Spec = rule.body.head
  val specB = rule.body.apply(1)

  def canProduce(insertee: Phrase, inserter: Head): Boolean = insertee.coreHead != null && inserter != null && RuleEngine.fit(insertee.coreHead, specA) && RuleEngine.fit(inserter, specB)

  def produce(insertee: Phrase, inserter: Head) {
    val map = new mutable.HashMap[String, Head]
    if (specA.id != null)
      map.put(specA.id, insertee.coreHead)
    if (specB.id != null)
      map.put(specB.id, inserter)
    map.foreach(pair => {
      val spec = rule.getSpecById(pair._1)
      if (spec != null && spec.operations.nonEmpty) {
        pair._2.performOperations(spec.operations, map)
      }
    })
    if (rule.product.operations.nonEmpty) {
      insertee.coreHead.performOperations(rule.product.operations, map)
    }
  }
}
