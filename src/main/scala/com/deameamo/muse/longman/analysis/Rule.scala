package com.deameamo.muse.longman.analysis

import com.deameamo.util.ArrayList

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

object Rule {
  val GNR = "GNR"
  val LEX = "LEX"
  val GRM = "GRM"
  val COL = "COL"
  val IDM = "IDM"
  val PHR = "PHR"
//  val MWE = "MWE"
  
  val GNR_PT = 1
  val LEX_PT = 2
  val GRM_PT = 3
  val COL_PT = 4
  val IDM_PT = 5
  val PHR_PT = 5
  val MWE_PT = 6
  val ERR_PT = -5
  val MIS_PT = -5
  
  val STR = "STR"
  val SEL = "SEL"
  val ALT = "ALT"
  val MWE = "MWE"
  
  val STR_FORM_SPEC_PT = 2
  val STR_TYPO_SPEC_PT = 1
  val UPG_FORM_SPEC_PT = 1
  val MWE_FORM_SPEC_PT = 3
  
  // status
  val PRIME = "PRIME"
  val RUNNING = "RUNNING"
  val DONE = "DONE"
  
  // direction
  val FORWARD = "FORWARD"
  val BACKWARD = "BACKWARD"
  val BOTH = "BOTH"
  
  def getPoint1(rule: Rule): Int = {
    rule.category match {
      case GNR => GNR_PT
      case LEX => LEX_PT
      case GRM => GRM_PT
      case COL => COL_PT
      case IDM => IDM_PT
      case PHR => PHR_PT
      case MWE => MWE_PT
    }
  }
  
  def getPoint(rule: Rule): Int = {
    var points = 0
    rule.category match {
//      case STR => {
//        rule.body.foreach(spec => {
//          if(!spec.forms.isEmpty) {
//            points += STR_FORM_SPEC_PT
//          }
//          else {
//            if(spec.stringAspects.contains(AspectName.QW_MARK))
//              points += STR_FORM_SPEC_PT
//            else
//              points += STR_TYPO_SPEC_PT
//          }
//        })
//      }
      case GNR => points = GNR_PT * (rule.body.size - 1)
      case LEX => points = LEX_PT * (rule.body.size - 1)
      case SEL => {
        rule.body.foreach(spec => {
          if(!spec.forms.isEmpty) {
            points += UPG_FORM_SPEC_PT
          }
        })
      }
      case ALT => {
        rule.body.foreach(spec => {
          if(!spec.forms.isEmpty) {
            points += UPG_FORM_SPEC_PT
          }
        })
      }
      case MWE => {
        rule.body.foreach(spec => {
          if(!spec.forms.isEmpty) {
            points += MWE_FORM_SPEC_PT
          }
        })
      }
    }
    points
  }
  
  def reverseDirection(direction: String): String = {
    direction match {
      case Rule.FORWARD => Rule.BACKWARD
      case Rule.BACKWARD => Rule.FORWARD
      case _ => null
    }
  }
}

class Rule(val category: String, val body: MutableList[Spec], val product: Spec) {
  var status: String = Rule.PRIME
  
  var from = -1
  var to = -1
  
  var lastDirection: String = null
  var lastNumber: Int = -1
  var lastHead: Head = null
  
  val fittedVector = new MutableList[Boolean]
  for(i <- 0 until body.size)
    fittedVector += false
  
  var numberOfMissingSpecs = body.size
  
  val headIdVector = new MutableList[String]
  for(i <- 0 until body.size)
    headIdVector += null
  
  val idMap = new HashMap[String, Spec]
  body.foreach(spec => {
    if(spec.id != null)
      idMap.put(spec.id, spec)
  })
  
  def string = s"${category}:${body.mkString(" ")}->${product}"
  
  def finish {
    from = 0
    to = body.size
    status = Rule.DONE
    numberOfMissingSpecs = 0
    for(i <- 0 until body.size)
      fittedVector.update(i, true)
  }
  
  def getCoreIndex: Int = {
    var index = -1
    var i = 0
    while(i < body.size && index == -1) {
      if(body.apply(i).isCore)
        index = i
      i += 1
    }
    index
  }
  
  def copy: Rule = {
    val peer = new Rule(category, body, product)
    peer.status = this.status
    peer.from = this.from
    peer.to = this.to
    for(i <- 0 until this.fittedVector.size) {
      peer.fittedVector.update(i, this.fittedVector.apply(i))
    }
    for(i <- 0 until this.headIdVector.size) {
      peer.headIdVector.update(i, this.headIdVector.apply(i))
    }
    peer.numberOfMissingSpecs = this.numberOfMissingSpecs
    peer.idMap ++= this.idMap
    peer.lastDirection = this.lastDirection
    peer.lastNumber = this.lastNumber
    peer.lastHead = this.lastHead
    peer
  }
  
  def fit(head: Head, direction: String): Spec = {
    if(head == null || status == Rule.DONE)
      return null
    lastDirection = direction
    var fits = false
    var number = 1
    var spec = getSpec(direction, number)
    while(fits == false && spec != null) {
      fits = RuleEngine.fit(head, spec)
      if(fits) {
        lastNumber = number
        lastHead = head
      }
      else {
        number += 1
        spec = getSpec(direction, number)
      }
    }
    if(fits)
      spec
    else
      null
  }
  
  def fitSpec(anther: Spec, direction: String): Boolean = {
    val spec = getSpec(direction, 1)
    if(spec != null)
      spec.weaklyEqualsTo(anther)
    else
      false
  }
  
  def getSpec(direction: String, number: Int): Spec = {
    if(status == Rule.DONE)
      return null
    direction match {
      case Rule.BACKWARD => {
        if(from - number >= 0)
          body.apply(from - number)
        else
          null
      }
      case Rule.FORWARD => {
        if(to + number <= body.size)
          body.apply(to + number - 1)
        else
          null
      }
    }
  }
  
  def updateStatus {
    lastDirection match {
      case Rule.BACKWARD => {
        from -= lastNumber
        fittedVector.update(from, true)
//        headIdVector.update(from, lastHead.id)
      }
      case Rule.FORWARD => {
        to += lastNumber
        fittedVector.update(to - 1, true)
//        headIdVector.update(to - 1, lastHead.id)
      }
    }
    numberOfMissingSpecs -= 1
    if(to - from == body.size)
      status = Rule.DONE
    else if(to - from == 1)
      status = Rule.PRIME
    else
      status = Rule.RUNNING
  }
  
  def getSpecById(id: String): Spec = {
    if(idMap.contains(id))
      idMap.apply(id)
    else
      null
  }
  
  def getMissingSpecs: MutableList[Spec] = {
    val list = new MutableList[Spec]
    for(i <- 0 until body.size) {
      if(fittedVector.apply(i) == false)
        list += body.apply(i)
    }
    list
  }
  
  def getMissingSpecIndex(target: Head): Int = {
    var index = -1
    var i = 0
    while(i < body.size && index == -1) {
      if(fittedVector.apply(i) == false) {
        if(RuleEngine.fit(target, body.apply(i))) {
          index = i
        }
      }
      i += 1
    }
    index
  }
  
  def isHigherThan(another: Rule): Boolean = Rule.getPoint(this) > Rule.getPoint(another)
  
  def equalsTo(another: Rule): Boolean = this.string == another.string && this.from == another.from && this.to == another.to && this.fittedVector.equals(another.fittedVector)
  
  override def toString = s"${category}:${body.mkString(" ")}->${product}[${from}~${to}] (${headIdVector.mkString(",")})"
  
  def createProductPhrase(corePhrase: Phrase): Phrase = {
    if( product.typo == corePhrase.coreHead.typo || (corePhrase.coreHead.typo == Head.CJP) && (product.typo == corePhrase.coreHead.get(AspectName.VAR_TYPO)) ) {
      corePhrase
    }
    else {
      product.typo match {
        case Head.DECL => {
          val declHead = new DECLHead(corePhrase.coreHead.form, corePhrase.coreHead.originality)
          declHead.id = Head.getSentenceHeadId
          declHead.init(corePhrase.coreHead)
          val declPhrase = PhraseFactory.create(this)
          declPhrase.initHead = corePhrase.initHead
          declPhrase.coreHead = declHead
          declHead.phrase = declPhrase
          declPhrase
        }
        case Head.INYN => {
          val inynHead = new INYNHead(corePhrase.coreHead.form, corePhrase.coreHead.originality)
          inynHead.id = Head.getSentenceHeadId
          inynHead.init(corePhrase.coreHead)
          val inynPhrase = PhraseFactory.create(corePhrase.rule)
          inynPhrase.coreHead = inynHead
          inynPhrase.isCorePhrase = true
          inynPhrase.headMap ++= corePhrase.headMap
          inynHead.phrase = inynPhrase
          inynPhrase
        }
        case Head.INWH => {
          val inwhHead = new INWHHead(corePhrase.coreHead.form, corePhrase.coreHead.originality)
          inwhHead.id = Head.getSentenceHeadId
          if(corePhrase.coreHead.typo == Head.INYN)
            inwhHead.performShallowCopy(corePhrase.coreHead)
          else
            inwhHead.init(corePhrase.coreHead)
          val inynPhrase = PhraseFactory.create(this)
          inynPhrase.coreHead = inwhHead
          inynPhrase.isCorePhrase = true
          inynPhrase.headMap ++= corePhrase.headMap
          inwhHead.phrase = inynPhrase
          inynPhrase
        }
        case Head.CLAUSE => {
          val clauseHead = new CLAUSEHead(corePhrase.coreHead.form, corePhrase.coreHead.originality)
          clauseHead.id = Head.getSentenceHeadId
          if(corePhrase.coreHead.typo == Head.DECL)
            clauseHead.performShallowCopy(corePhrase.coreHead)
          else
            clauseHead.init(corePhrase.coreHead)
          val clausePhrase = PhraseFactory.create(this)
          clausePhrase.units ++= corePhrase.units
//          clausePhrase.initHead = corePhrase.initHead
          clausePhrase.coreHead = clauseHead
          clausePhrase.isCorePhrase = true
          clausePhrase.headMap ++= corePhrase.headMap
          clauseHead.phrase = clausePhrase
          clausePhrase
        }
        case Head.CJP => {
          val phrase = PhraseFactory.create(this)
          val cjpHead = new CJPHead(corePhrase.coreHead.form, corePhrase.coreHead.originality)
          cjpHead.phrase = phrase
          phrase.units ++= corePhrase.units
//          phrase.initHead = corePhrase.initHead
          phrase.coreHead = cjpHead
          phrase.coreHead.id = Head.getCoreHeadId
          phrase
        }
        case POS.NOUN => {
          println(s"find noun, corePhrase (change Head.PPP back to Head.NNP): corePhrase.coreHead.typo=${corePhrase.coreHead.typo}")
          corePhrase
//          if(corePhrase.coreHead.typo == Head.PPP)
//            corePhrase
//          else
//            null
        }
        case POS.VERB => {
//          println(s"find verb, corePhrase: ${corePhrase}")
          corePhrase
        }
        case POS.ADJECTIVE => {
          println(s"find ADJECTIVE, corePhrase: ${corePhrase}")
          corePhrase
        }
        case Head.ERROR => null
        case _ => null
      }
    }
  }
  
  def isComplete: Boolean = {
    var allFitted = true
    var i = 0
    while(i < fittedVector.size && allFitted) {
      allFitted = fittedVector.apply(i)
      i += 1
    }
    allFitted
  }
  
  def clearHeadIdVector {
    for(i <- 0 until headIdVector.size) {
      headIdVector.update(i, null)
    }
  }
}

class CompoundRule(category: String, body: List[BaseSpec], product: Spec) {
  val rules = new MutableList[Rule]
  
  private val ruleBody = new MutableList[Spec]
  for(i <- 0 until body.size) {
    val iter = body.apply(i)
    if(iter.isInstanceOf[Spec])
      ruleBody += null
    else if(iter.isInstanceOf[OptionalSpec]) {
      for(i <- 0 until iter.asInstanceOf[OptionalSpec].specs.size) {
        ruleBody += null
      }
    }
    else {
      ruleBody += null
      ruleBody += null
    }
  }
  
  collect(ruleBody, 0, 0)
  
  private def collect(ruleBody: MutableList[Spec], majorIndex: Int, size: Int) {
    if(majorIndex == body.size) {
      rules += new Rule(category, ruleBody.take(size), product)
    }
    else {
      val iter = body.apply(majorIndex)
      if(iter.isInstanceOf[Spec]) {
        ruleBody.update(size, iter.asInstanceOf[Spec])
        collect(ruleBody, majorIndex + 1, size + 1)
      }
      else if(iter.isInstanceOf[OptionalSpec]) {
        for(minorIndex <- 0 until 2) {
          val peer = fork(ruleBody)
          collect(peer, majorIndex + 1, iter.asInstanceOf[OptionalSpec].inject(peer, minorIndex, size))
        }
      }
      else if(iter.isInstanceOf[SwitchableSpec]) {
        for(minorIndex <- 0 until 2) {
          val peer = fork(ruleBody)
          collect(peer, majorIndex + 1, iter.asInstanceOf[SwitchableSpec].inject(peer, minorIndex, size))
        }
      }
    }
  }
  
  private def fork(ruleBody: MutableList[Spec]): MutableList[Spec] = {
    val peer = new MutableList[Spec]
    ruleBody.foreach(peer += _)
    peer
  }
  
  override def toString = s"${body.mkString(" ")}->${product}"
}

class BaseSpec

class Spec(
    val isCore: Boolean,
    val forms: MutableList[String],
    var typo: String,
    val values: ArrayList[String],
    val expectation: Spec,
    val level: Int,
    val id: String,
    val operations: MutableList[Operation]) extends BaseSpec {
  
  val stringAspects = new HashMap[String, String]
  val headAspects = new HashMap[String, String]
  val listAspects = new HashMap[String, Int]
  
  val toBeRemovedList = new MutableList[String]
  
  values.foreach(value => {
    value match {
      // typo
      case Head.NNP => typo = Head.NNP; toBeRemovedList += value
      case Head.PPP => typo = Head.PPP; toBeRemovedList += value
      case Head.PN => typo = Head.PN; toBeRemovedList += value
      case Head.DT => typo = Head.DT; toBeRemovedList += value
      case Head.CJP => typo = Head.CJP; toBeRemovedList += value
      // for NNP
      case AspectValue.NOM => stringAspects.put(AspectName.ROLE, AspectValue.NOM)
      case AspectValue.OBL => stringAspects.put(AspectName.ROLE, AspectValue.OBL)
      case AspectValue.REFL => stringAspects.put(AspectName.ROLE, AspectValue.REFL)
      case AspectValue.POSS => stringAspects.put(AspectName.ROLE, AspectValue.POSS)
      case AspectValue.SBSTH => stringAspects.put(AspectName.GENRE, AspectValue.SBSTH)
      case AspectValue.SB => stringAspects.put(AspectName.GENRE, AspectValue.SB)
      case AspectValue.STH => stringAspects.put(AspectName.GENRE, AspectValue.STH)
      case AspectValue.P3SING => stringAspects.put(AspectName.PERSON_NUMBER, AspectValue.P3SING)
      case AspectValue.P3PL => stringAspects.put(AspectName.PERSON_NUMBER, AspectValue.P3PL)
      case AspectValue.CNT => stringAspects.put(AspectName.COUNTABILITY, AspectValue.CNT)
      case AspectValue.NODT => headAspects.put(AspectName.DET, AspectValue.NODT)
      case AspectValue.DA => headAspects.put(AspectName.DET, AspectValue.DA)
      case AspectValue.IA => headAspects.put(AspectName.DET, AspectValue.IA)
      case AspectValue.NOMOD => {
        listAspects.put(AspectName.PREMOD, 0)
        listAspects.put(AspectName.POSTMOD, 0)
      }
      case AspectValue.NOPREMOD => listAspects.put(AspectName.PREMOD, 0)
      case AspectValue.NOPOSTMOD => listAspects.put(AspectName.POSTMOD, 0)
      // for VBP
      case AspectValue.STEM => stringAspects.put(AspectName.IS_STEM, AspectValue.STEM)
      case AspectValue.INFINITIVE => stringAspects.put(AspectName.FUNCTION, AspectValue.INFINITIVE)
      case AspectValue.PREDICATE => stringAspects.put(AspectName.FUNCTION, AspectValue.PREDICATE)
      case AspectValue.PRESPART => stringAspects.put(AspectName.FUNCTION, AspectValue.PRESPART)
      case AspectValue.PASTPART => stringAspects.put(AspectName.FUNCTION, AspectValue.PASTPART)
      case AspectValue.ACT => stringAspects.put(AspectName.VOICE, AspectValue.ACT)
      case AspectValue.PASS => stringAspects.put(AspectName.VOICE, AspectValue.PASS)
      case AspectValue.NEG => stringAspects.put(AspectName.POSITIVITY, AspectValue.NEG)
      case AspectValue.PROG => stringAspects.put(AspectName.CONTINUANCE, AspectValue.PROG)
      case AspectValue.PERF => stringAspects.put(AspectName.CONTINUANCE, AspectValue.PERF)
      case AspectValue.PRES => stringAspects.put(AspectName.TENSE, AspectValue.PRES)
      case AspectValue.PAST => stringAspects.put(AspectName.TENSE, AspectValue.PAST)
      case AspectValue.FUT => stringAspects.put(AspectName.TENSE, AspectValue.FUT)
      case AspectValue.POS => headAspects.put(AspectName.POSITIVITY, AspectValue.POS)
      case AspectValue.NOT => headAspects.put(AspectName.POSITIVITY, AspectValue.NOT)
      case AspectValue.NOMD => headAspects.put(AspectName.MODALITY, AspectValue.NOMD)
      // for QW question word
      case AspectValue.WHAT => stringAspects.put(AspectName.QW_MARK, AspectValue.WHAT)
      case AspectValue.WHOM => stringAspects.put(AspectName.QW_MARK, AspectValue.WHOM)
      case AspectValue.THAT => stringAspects.put(AspectName.QW_MARK, AspectValue.THAT)
    }
  })
  
  values.removeElemes(toBeRemovedList)
  
  def weaklyEqualsTo(another: Spec): Boolean = {
    this.typo == another.typo && {
      if(!this.forms.isEmpty && !another.forms.isEmpty) {
        var is = false
        var i = 0
        while(i < another.forms.size && !is) {
          is = this.forms.contains(another.forms.apply(i))
          i += 0
        }
        is
      }
      else
        true
    }
  }
  
  override def toString = 
    (if(isCore) "*" else "") +
    forms.mkString("/") +
    (if(typo != Head.UNDEF) s"{${typo}}" else "") +
    (if(!values.isEmpty) values.mkString("[", ",", "]") else "") +
    (if(expectation != null) s"<${expectation}>" else "") +
    (if(level != 0) s"#${level}" else "") +
    (if(id != null) s"$$${id}" else "") +
    (if(!operations.isEmpty) operations.mkString("^", ",", "") else "")
}

class OptionalSpec(val specs: List[Spec]) extends BaseSpec {
  def inject(ruleBody: MutableList[Spec], minorIndex: Int, size: Int): Int = {
    if(minorIndex == 0) {
      for(i <- 0 until specs.size) {
        ruleBody.update(size + i, specs.apply(i))
      }
      size + specs.size
    }
    else {
      size
    }
  }
  
  override def toString = specs.mkString("(", " ", ")")
}

class SwitchableSpec(val specA: Spec, val specB: Spec) extends BaseSpec {
  def inject(ruleBody: MutableList[Spec],  minorIndex: Int, size: Int): Int = {
    if(minorIndex == 0) {
      ruleBody.update(size, specA)
      ruleBody.update(size + 1, specB)
    }
    else {
      ruleBody.update(size, specB)
      ruleBody.update(size + 1, specA)
    }
    size + 2
  }
  
  override def toString = s"(~${specA} ↔ ${specB}~)"
}

class Operation1(val operator: String, var operandA: String, var operandB: String) {
  override def toString = s"(${operator}:${operandA},${operandB})"
}

class Operation(val operator: String) {
  val operands = new MutableList[String]
  override def toString = s"(${operator}:[${operands.mkString(",")}])"
}

object Operator {
  // "@"
  val ASSIGN = "ASSIGN"
  // "="
  val SET = "SET"
  // "+"
  val ADD = "ADD"
  // "."
  val REPLACE = "REPLACE"
  // "~"
  val CHECK = "CHECK"
  // ">"
  val INSERT = "INSERT"
  // "%"
  val JOIN = "JOIN"
  
  val DROP = "DROP"
}
