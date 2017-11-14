package com.deameamo.muse.longman.analysis

import scala.collection.mutable

object Head {
  val NNP = "NNP"
  val VBP = "VBP"
  val VBM = "VBM"
  val VBA = "VBA"
  val AJE = "AJE"
  val AVE = "AVE"
  val PPP = "PPP"
  val CJP = "CJP"
  val NODT = "NODT"
  val NOMD = "NOMD"
  val NOPD = "NOPD"
  val NOTARGET = "NOTARGET"
  val POS = "POS"
  val IA = "IA"
  val DT = "DT"
  val PN = "PN"
  val CJ = "CJ"
  val UNDEF = "UNDEF"
  val DECL = "DECL"
  val INYN = "INYN"
  val INWH = "INWH"
  val CLAUSE = "CLAUSE"
  val COMP = "COMP"
  val ANY = "ANY"
  val ERROR = "ERROR"
  val NNS = "NNS"
  val NNL = "NNL"

  val NODTHead = new Head("", Head.NODT)
  val NOPDHead = new Head("", Head.NOPD)
  val NOMDHead = new Head("", Head.NOMD)
  val NOTARGETHead = new Head("", Head.NOTARGET)
  val POSHead = new Head("", Head.POS)
  val UNDEFHead = new Head("", Head.UNDEF)

  val SHOW_ID = true

  private var coreHeadIndex = 0
  private var initHeadIndex = 0
  private var sentenceHeadIndex = 0

  private val indentCache = new mutable.HashMap[Int, String]

  def getIndent(level: Int, prefix: String): String = {
    if (!indentCache.contains(level)) {
      val list = new mutable.MutableList[String]
      for (_ <- 0 until level)
        list += "  "
      indentCache.put(level, list.mkString)
    }
    indentCache.apply(level) + (if (prefix.length > 0) prefix + ":" else "")
  }

  def getCoreHeadId: String = {
    coreHeadIndex += 1
    s"c$coreHeadIndex"
  }

  def getInitHeadId: String = {
    initHeadIndex += 1
    s"i$initHeadIndex"
  }

  def getSentenceHeadId: String = {
    sentenceHeadIndex += 1
    s"s$sentenceHeadIndex"
  }
}

class Head(val form: String, val typo: String, val originality: String) {
  def this(form: String, typo: String) = this(form, typo, AspectValue.ORIG)

  var id = "i"

  var phrase: Phrase = _

  val stringAspects = new mutable.HashMap[String, String]

  val headAspects = new mutable.HashMap[String, Head]

  val variables = new mutable.MutableList[Head]

  val preModifiers = new mutable.MutableList[Head]

  val postModifiers = new mutable.MutableList[Head]

  var quality = 0

  set(AspectName.QW_TYPE, AspectValue.PLAIN)
  set(AspectName.QW_MARK, AspectValue.PLAIN)

  def getChildInLevel(spec: Spec, level: Int, direction: String): Head = {
    var child: Head = null
    direction match {
      case Rule.FORWARD =>
        if (level == 0) {
          child = getChildInList(spec, variables)
          if (child == null)
            child = getChildInList(spec, postModifiers)
        }
        else {
          child = getChildInLevelInList(spec, level - 1, variables)
          if (child == null)
            child = getChildInLevelInList(spec, level - 1, postModifiers)
        }
      case Rule.BACKWARD =>
        if (level == 0) {
          child = getChildInList(spec, preModifiers)
        }
        else {
          child = getChildInLevelInList(spec, level - 1, preModifiers)
        }
      case Rule.BOTH =>
        if (level == 0) {
          child = getChildInList(spec, variables)
          if (child == null)
            child = getChildInList(spec, postModifiers)
          if (child == null)
            child = getChildInList(spec, preModifiers)
        }
        else {
          child = getChildInLevelInList(spec, level - 1, variables)
          if (child == null)
            child = getChildInLevelInList(spec, level - 1, postModifiers)
          if (child == null)
            child = getChildInLevelInList(spec, level - 1, preModifiers)
        }
    }
    child
  }

  private def getChildInList(spec: Spec, list: mutable.MutableList[Head]): Head = {
    var child: Head = null
    var i = 0
    while (i < list.size && child == null) {
      if (RuleEngine.fit(list.apply(i), spec)) {
        child = list.apply(i)
      }
      i += 1
    }
    child
  }

  private def getChildInLevelInList(spec: Spec, level: Int, list: mutable.MutableList[Head]): Head = {
    var child: Head = null
    var i = 0
    while (i < list.size && child == null) {
      child = list.apply(i).getChildInLevel(spec, level, Rule.BOTH)
      i += 1
    }
    child
  }

  def childExistsInLevel(spec: Spec, level: Int, direction: String): Boolean = {
    direction match {
      case Rule.FORWARD =>
        if (level == 0) {
          specExistsInList(spec, variables) || specExistsInList(spec, postModifiers)
        }
        else {
          childExistsInList(spec, level - 1, variables) || childExistsInList(spec, level - 1, postModifiers)
        }
      case Rule.BACKWARD =>
        if (level == 0) {
          specExistsInList(spec, preModifiers)
        }
        else {
          childExistsInList(spec, level - 1, preModifiers)
        }
      case Rule.BOTH =>
        if (level == 0) {
          specExistsInList(spec, variables) ||
            specExistsInList(spec, preModifiers) ||
            specExistsInList(spec, postModifiers)
        }
        else {
          childExistsInList(spec, level - 1, variables) ||
            childExistsInList(spec, level - 1, preModifiers) ||
            childExistsInList(spec, level - 1, postModifiers)
        }
    }
  }

  private def specExistsInList(spec: Spec, list: mutable.MutableList[Head]): Boolean = {
    var exists = false
    var i = 0
    while (i < list.size && !exists) {
      exists = RuleEngine.fit(list.apply(i), spec)
      i += 1
    }
    exists
  }

  private def childExistsInList(spec: Spec, level: Int, list: mutable.MutableList[Head]): Boolean = {
    var exists = false
    var i = 0
    while (i < list.size && !exists) {
      exists = list.apply(i).childExistsInLevel(spec, level, Rule.BOTH)
      i += 1
    }
    exists
  }

  def shallowCopy(refPhrase: Phrase): Head = {
    val peer = new Head(form, typo, originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  def performShallowCopy(another: Head) {
    this.stringAspects.clear
    this.headAspects.clear
    this.variables.clear
    this.preModifiers.clear
    this.postModifiers.clear
    this.stringAspects ++= another.stringAspects
    this.headAspects ++= another.headAspects
    //    another.variables.foreach(v => this.variables += v.shallowCopy(v.phrase))
    //    another.preModifiers.foreach(m => this.preModifiers += m.shallowCopy(m.phrase))
    //    another.postModifiers.foreach(m => this.postModifiers += m.shallowCopy(m.phrase))
    this.variables ++= another.variables
    this.preModifiers ++= another.preModifiers
    this.postModifiers ++= another.postModifiers
    this.quality = another.quality
  }

  def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): Head = {
    val peer = new Head(form, typo, originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }

  def performDeepCopy(another: Head, peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]) {
    def getPeerPhrase(source: Phrase): Phrase = {
      if (!peerPhraseMap.contains(source.id)) {
        peerPhraseMap.put(source.id, source.shallowCopy)
      }
      peerPhraseMap.apply(source.id)
    }

    this.id = another.id
    peerHeadMap.put(this.id, this)
    this.phrase = getPeerPhrase(another.phrase)
    this.quality = another.quality
    this.stringAspects ++= another.stringAspects
    //    this.headAspects ++= another.headAspects
    //    this.headAspects.foreach(headAspect => {
    //      peerHeadMap.put(headAspect._2.id, headAspect._2)
    //    })
    another.headAspects.foreach(headAspect => {
      if (headAspect._2.phrase != null) {
        headAspect._2.deepCopy(peerHeadMap, peerPhraseMap)
        this.headAspects.put(headAspect._1, peerHeadMap.apply(headAspect._2.id))
      }
      else {
        this.headAspects += headAspect
      }
    })
    another.variables.foreach(v => this.variables += v.deepCopy(peerHeadMap, peerPhraseMap))
    another.preModifiers.foreach(m => this.preModifiers += m.deepCopy(peerHeadMap, peerPhraseMap))
    another.postModifiers.foreach(m => this.postModifiers += m.deepCopy(peerHeadMap, peerPhraseMap))
    if (another.phrase.initHead != null && !peerHeadMap.contains(another.phrase.initHead.id)) {
      Debugger.println(s"another.phrase.initHead=${another.phrase.initHead.phrase.id}")
      another.phrase.initHead.deepCopy(peerHeadMap, peerPhraseMap)
    }
    another.phrase.headMap.foreach(pair => {
      if (!peerHeadMap.contains(pair._2.id)) {
        pair._2.deepCopy(peerHeadMap, peerPhraseMap)
      }
    })
  }

  def replaceHead(peerHeadMap: mutable.HashMap[String, Head]) {
    if (phrase.initHead != null) {
      phrase.initHead = peerHeadMap.apply(phrase.initHead.id)
    }
    if (phrase.coreHead != null) {
      phrase.coreHead = peerHeadMap.apply(phrase.coreHead.id)
    }
    phrase.headMap.foreach(pair => {
      if (peerHeadMap.contains(pair._2.id))
        phrase.headMap.update(pair._1, peerHeadMap.apply(pair._2.id))
      else {
        println(s"replaceHead else: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        val peer = pair._2.phrase.deepCopy
        if (pair._2.phrase.coreHead != null) {
          phrase.headMap.update(pair._1, peer.coreHead)
        }
        else {
          phrase.headMap.update(pair._1, peer.initHead)
        }
      }
    })
    variables.foreach(_.replaceHead(peerHeadMap))
    preModifiers.foreach(_.replaceHead(peerHeadMap))
    postModifiers.foreach(_.replaceHead(peerHeadMap))
  }

  protected def rewriteAspects(): Unit = {}

  def mergeWith(another: Head) {
    this.phrase.units ++= another.phrase.units
    for (i <- this.variables.indices) {
      this.variables.apply(i).mergeWith(another.variables.apply(i))
    }
    for (i <- this.preModifiers.indices) {
      this.preModifiers.apply(i).mergeWith(another.preModifiers.apply(i))
    }
    for (i <- this.postModifiers.indices) {
      this.postModifiers.apply(i).mergeWith(another.postModifiers.apply(i))
    }
  }

  def fitsWith(another: Head): Boolean = this.form == another.form && this.typo == another.typo

  def equalsTo(another: Head): Boolean = equalsTo(another, rewritingAspects = false)

  def equalsTo(another: Head, rewritingAspects: Boolean): Boolean = {
    if (rewritingAspects) {
      this.rewriteAspects()
      another.rewriteAspects()
    }
    this.form == another.form &&
      this.typo == another.typo &&
      this.quality == another.quality &&
      this.phrase.rule.string == another.phrase.rule.string &&
      equalsInStringAspects(another) &&
      equalsInHeadAspects(another, rewritingAspects) &&
      equalsInVariables(another, rewritingAspects) &&
      equalsInPreModifiers(another, rewritingAspects) &&
      equalsInPostModifiers(another, rewritingAspects)
  }

  def isPure: Boolean = variables.isEmpty && preModifiers.isEmpty && postModifiers.isEmpty

  def generateId(id: String) {
    this.id = id
    println(s"generateId: phrase.coreHead.id=${phrase.coreHead.id}, id=$id," +
      s"phrase.coreHead.form=${phrase.coreHead.form}, this.form=${this.form}, ${phrase.coreHead == this}")
    for (i <- variables.indices) {
      variables.apply(i).generateId(s"${id}_v$i")
    }
    for (i <- preModifiers.indices) {
      preModifiers.apply(i).generateId(s"${id}_-$i")
    }
    for (i <- postModifiers.indices) {
      postModifiers.apply(i).generateId(s"${id}_+$i")
    }
  }

  // operations ===============================================================
  def performOperationsInComplexHead(operations: mutable.MutableList[Operation], headMap: mutable.HashMap[String, Head]) {
    operations.foreach(operation => {
      operation.operator match {
        case Operator.SET => set(operation.operands, headMap)
        case Operator.ASSIGN => assign(operation.operands, headMap)
        case Operator.ADD => addInComplexHead(operation.operands, headMap)
        case Operator.REPLACE => replace(operation.operands, headMap)
        case Operator.CHECK => check(operation.operands, headMap)
        case _ =>
      }
    })
    onOperationsPerformed()
  }

  private def addInComplexHead(operands: mutable.MutableList[String], headMap: mutable.HashMap[String, Head]) {
    // A.list_name+head_id
    if (operands.size == 3) {
      if (headMap.contains(operands.head) && headMap.contains(operands.apply(2)))
        headMap.apply(operands.head).performAdding(operands.apply(1), headMap.apply(operands.apply(2)))
    }
    // list_name+head_id
    else {
      if (headMap.contains(operands.apply(1)))
        performAdding(operands.head, headMap.apply(operands.apply(1)))
    }
  }

  def performAdding(listName: String, head: Head) {
    if (typo == Head.CJP) {
      markQW(head)
      variables.foreach(variable => {
        //        val headPhrasePeer = head.phrase.deepCopy
        val headPeer = head.phrase.peerHeadMap.apply(head.id)
        variable.add(listName, headPeer)
      })
    }
    else {
      add(listName, head)
    }
  }

  def performOperations(operations: mutable.MutableList[Operation], headMap: mutable.HashMap[String, Head]): Boolean = {
    var successful = true
    operations.foreach(operation => {
      if (successful) {
        operation.operator match {
          case Operator.SET => set(operation.operands, headMap)
          case Operator.ASSIGN => assign(operation.operands, headMap)
          case Operator.ADD => add(operation.operands, headMap)
          case Operator.REPLACE => replace(operation.operands, headMap)
          case Operator.CHECK => check(operation.operands, headMap)
          case Operator.JOIN => successful = join(operation.operands, headMap)
          case _ =>
        }
      }
    })
    onOperationsPerformed()
    successful
  }

  protected def onOperationsPerformed() {}

  def set(name: String, value: String) {
    stringAspects.put(name, value)
  }

  def assign(name: String, head: Head) {
    markQW(head)
    headAspects.put(name, head)
  }

  private def set(operands: mutable.MutableList[String], headMap: mutable.HashMap[String, Head]) {
    // A.string_aspect_name=string_value
    if (operands.size == 3) {
      if (headMap.contains(operands.head))
        headMap.apply(operands.head).set(operands.apply(1), operands.apply(2))
    }
    // string_aspect_name=string_value
    else if (operands.size == 2) {
      set(operands.head, operands.apply(1))
    }
  }

  private def assign(operands: mutable.MutableList[String], headMap: mutable.HashMap[String, Head]) {
    // A.head_aspect_name@head_id
    if (operands.size == 3) {
      if (headMap.contains(operands.head) && headMap.contains(operands.apply(2)))
        headMap.apply(operands.head).assign(operands.apply(1), headMap.apply(operands.apply(2)))
    }
    // head_aspect_name@head_id
    else if (operands.size == 2) {
      if (headMap.contains(operands.apply(1)))
        assign(operands.head, headMap.apply(operands.apply(1)))
    }
  }

  private def add(operands: mutable.MutableList[String], headMap: mutable.HashMap[String, Head]) {
    // A.list_name+head_id
    if (operands.size == 3) {
      if (headMap.contains(operands.head) && headMap.contains(operands.apply(2)))
        headMap.apply(operands.head).add(operands.apply(1), headMap.apply(operands.apply(2)))
    }
    // list_name+head_id
    else {
      if (headMap.contains(operands.apply(1)))
        add(operands.head, headMap.apply(operands.apply(1)))
    }
  }

  def add(listName: String, head: Head) {
    markQW(head)
    listName match {
      case AspectName.VAR =>
        //        head.prependPrefix(s"${id}v${variables.size}")
        variables += head
        quality += head.quality
      case AspectName.PREMOD =>
        //        head.prependPrefix(s"${id}b${preModifiers.size}")
        preModifiers += head
        quality += head.quality
      case AspectName.POSTMOD =>
        //        head.prependPrefix(s"${id}a${postModifiers.size}")
        postModifiers += head
        quality += head.quality
      case _ =>
    }
  }

  def prependPrefix(prefix: String) {
    id = prefix + id
    variables.foreach(_.prependPrefix(prefix))
    preModifiers.foreach(_.prependPrefix(prefix))
    postModifiers.foreach(_.prependPrefix(prefix))
  }

  private def markQW(head: Head) {
    if (head.get(AspectName.QW_MARK) != AspectValue.PLAIN) {
      set(AspectName.QW_MARK, head.get(AspectName.QW_MARK))
    }
  }

  private def replace(operands: mutable.MutableList[String], headMap: mutable.HashMap[String, Head]) {
    val another = headMap.apply(operands.head)
    val name = operands.apply(1)
    // A.aspect_name
    if (stringAspects.contains(name))
      stringAspects.update(name, another.get(name))
    else if (headAspects.contains(name)) {
      markQW(another.read(name))
      headAspects.update(name, another.read(name))
    }
  }

  def check(operands: mutable.MutableList[String], headMap: mutable.HashMap[String, Head]) {
    val a = headMap.apply(operands.head)
    val b = headMap.apply(operands.apply(1))
    // A~B
    val perNumA = a.get(AspectName.PERSON_NUMBER)
    val perNumB = b.get(AspectName.PERSON_NUMBER)
    if (perNumA != null && perNumB != null) {
      perNumA match {
        case AspectValue.P1SING => if (perNumB != AspectValue.P1SING && perNumB != AspectValue.COMMON && perNumB != AspectValue.ALL) quality += Rule.ERR_PT
        case AspectValue.P1PL => if (perNumB != AspectValue.P1PL && perNumB != AspectValue.COMMON && perNumB != AspectValue.ALL) quality += Rule.ERR_PT
        case AspectValue.P2 => if (perNumB != AspectValue.P2 && perNumB != AspectValue.COMMON && perNumB != AspectValue.ALL) quality += Rule.ERR_PT
        case AspectValue.P3SING => if (perNumB != AspectValue.P3SING && perNumB != AspectValue.ALL) quality += Rule.ERR_PT
        case AspectValue.P3PL => if (perNumB != AspectValue.P3PL && perNumB != AspectValue.COMMON && perNumB != AspectValue.ALL) quality += Rule.ERR_PT
        case AspectValue.P3BI => if (perNumB != AspectValue.P3SING && perNumB != AspectValue.P3PL && perNumB != AspectValue.COMMON && perNumB != AspectValue.ALL) quality += Rule.ERR_PT
        case AspectValue.COMMON => if (perNumB == AspectValue.P3SING) quality -= Rule.MIS_PT
        case _ =>
      }
    }
    //    println(s"check: perNumA=${perNumA}, perNumB=${perNumB}, errCount=${errCount}")
  }

  def join(operands: mutable.MutableList[String], headMap: mutable.HashMap[String, Head]): Boolean = {
    var successful = false
    val headA = headMap.apply(operands.head)
    val headB = headMap.apply(operands.apply(1))
    if (headA.phrase.isCorePhrase != headB.phrase.isCorePhrase)
      return false
    // LEX:{ANY}$A *and[CJP]$B->{CJP}^A%B,VAR+A
    if (headB.typo == Head.CJP) {

    }
    // LEX:{ANY}$A *and {ANY}$B->{CJP}^A%B,VAR+A,VAR+B
    else {
      val varTypo: String = {
        if (headA.typo == headB.typo)
          headB.typo
        else
          RuleEngine.getEquiHeadTypo(headA, headB)
      }
      if (varTypo != null) {
        this.asInstanceOf[CJPHead].assimilateAspects(varTypo, headA, headB)
        if (!headB.phrase.isCorePhrase) {
          val headRule = this.asInstanceOf[CJPHead].generateHeadRule(headA, headB)
          //          println(s"join: headRule=${headRule}, ruleA=${headA.phrase.rule}, ruleB=${headB.phrase.rule}")
          if (headRule != null) {
            headB.phrase.headMap.foreach(pair => {
              println(s"${pair._1} => ${pair._2.form}[${pair._2.id}], ${pair._2.id == headB.id}")
              if (pair._2.id == headB.id)
                this.phrase.headMap.put(pair._1, this)
              else
                this.phrase.headMap.put(pair._1, pair._2)
            })
            phrase.coreHead = null
            phrase.initHead = this
            phrase.isCorePhrase = false
            phrase.rule = headRule
            successful = true
          }
          else
            successful = false
        }
        else {
          phrase.isCorePhrase = true
          successful = true
        }
      }
    }
    successful
  }

  def get(name: String): String = stringAspects.getOrElse(name, null)

  def read(name: String): Head = headAspects.getOrElse(name, null)

  //===========================================================================

  override def toString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
    s"$form[$typo,${get(AspectName.QW_MARK)}]" +
    s"${if (phrase != null) s",missing:${phrase.rule.getMissingSpecs.mkString("<", ",", ">")}" else ""}"

  def toSimpleString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}$form[$typo]"

  def print() {
    print(0, "ROOT")
  }

  def print(level: Int, prefix: String) {
    println(Head.getIndent(level, prefix) + toString + s"${phrase.units.mkString("[", ",", "]")}")
    variables.foreach(variable => {
      variable.print(level + 1, "VAR")
    })
    var i = preModifiers.size - 1
    while (i >= 0) {
      preModifiers.apply(i).print(level + 1, "PREMOD")
      i -= 1
    }
    postModifiers.foreach(modifier => {
      modifier.print(level + 1, "POSTMOD")
    })
  }

  def insertGenerally(inserter: Head, wholePhrase: Phrase, list: mutable.MutableList[Phrase], parent: Head) {
    if (phrase.typo == Phrase.STRUCTURER) {
      val index = {
        if (parent != null && this.typo == Head.PPP && parent.typo != Head.VBP)
          -1
        else
          phrase.rule.getMissingSpecIndex(inserter)
      }
      if (index != -1) {
        //        println(s"can insert ${inserter.form}[${inserter.typo}] into (${this.id})${this.form}[${this.typo}]")
        val wholePhrasePeer = wholePhrase.deepCopy
        //        println(s"insert: wholePhrase.copiedHeadMap=${wholePhrase.copiedHeadMap}")
        val inserteePeer = wholePhrase.peerHeadMap.apply(id)
        //        println(s"insert: insertee=${insertee}")
        //        println(s"insert: insertee.phrase.headMap=${insertee.phrase.headMap}")
        var gain = inserteePeer.phrase.asInstanceOf[StructurerPhrase].insertAsMissing(inserter, index)
        //        println(s"insert: inserteePeer.phrase.rule=${inserteePeer.phrase.rule}")
        //        println(s"insert: gain=${gain}, insertee=${insertee.toSimpleString}, rule=${this.phrase.rule}")
        wholePhrasePeer.coreHead.quality += gain
        //        list += wholePhrasePeer
        val originalListSize = list.size
        wholePhrasePeer.asInstanceOf[StructurerPhrase].upgradeChildren(wholePhrasePeer, list)
        if (list.size == originalListSize)
          list += wholePhrasePeer
      }
      else {
        val insertionRule = RuleEngine.canInsertByInsertionRules(phrase, inserter)
        if (insertionRule != null) {
          //          println(s"insert (can insert by birule): insertee=(${this.id})${this.form}[${this.typo}], inserter=${inserter.form}[${inserter.typo}]")
          val wholePhrasePeer = wholePhrase.deepCopy
          val inserteePeer = wholePhrase.peerHeadMap.apply(id)
          //          println(s"insert: wholePhrasePeer")
          //          wholePhrasePeer.coreHead.print
          var gain = inserteePeer.phrase.asInstanceOf[StructurerPhrase].insertByInsertionRule(inserter, insertionRule)
          //          println(s"insert: gain=${gain}")
          wholePhrasePeer.coreHead.quality += gain
          val originalListSize = list.size
          wholePhrasePeer.asInstanceOf[StructurerPhrase].upgradeChildren(wholePhrasePeer, list)
          if (list.size == originalListSize)
            list += wholePhrasePeer
        }
      }
    }

    for (i <- variables.indices) {
      variables.apply(i).insertGenerally(inserter, wholePhrase, list, this)
    }
    for (i <- preModifiers.indices) {
      preModifiers.apply(i).insertGenerally(inserter, wholePhrase, list, this)
    }
    for (i <- postModifiers.indices) {
      postModifiers.apply(i).insertGenerally(inserter, wholePhrase, list, this)
    }
  }

  private def equalsInStringAspects(another: Head): Boolean = {
    var equals = this.stringAspects.size == another.stringAspects.size
    if (equals) {
      this.stringAspects.foreach(pair => {
        if (equals) {
          equals = pair._2 == another.get(pair._1)
        }
      })
    }
    equals
  }

  private def equalsInHeadAspects(another: Head, rewritingAspects: Boolean): Boolean = {
    var equals = this.headAspects.size == another.headAspects.size
    if (equals) {
      this.headAspects.foreach(pair => {
        if (equals) {
          val thisHead = pair._2
          val anotherHead = another.read(pair._1)
          equals = thisHead.form == anotherHead.form && thisHead.typo == anotherHead.typo && thisHead.quality == anotherHead.quality &&
            (if (thisHead.phrase != null && anotherHead.phrase != null) thisHead.phrase.rule.string == anotherHead.phrase.rule.string else true)
        }
      })
    }
    equals
  }

  private def equalsInVariables(another: Head, rewritingAspects: Boolean): Boolean = {
    var equals = this.variables.size == another.variables.size
    if (equals) {
      var i = 0
      while (i < this.variables.size && equals) {
        equals = this.variables.apply(i).equalsTo(another.variables.apply(i), rewritingAspects)
        i += 1
      }
    }
    equals
  }

  private def equalsInPreModifiers(another: Head, rewritingAspects: Boolean): Boolean = {
    var equals = this.preModifiers.size == another.preModifiers.size
    if (equals) {
      var i = 0
      while (i < this.preModifiers.size && equals) {
        equals = this.preModifiers.apply(i).equalsTo(another.preModifiers.apply(i), rewritingAspects)
        i += 1
      }
    }
    equals
  }

  private def equalsInPostModifiers(another: Head, rewritingAspects: Boolean): Boolean = {
    var equals = this.postModifiers.size == another.postModifiers.size
    if (equals) {
      var i = 0
      while (i < this.postModifiers.size && equals) {
        equals = this.postModifiers.apply(i).equalsTo(another.postModifiers.apply(i), rewritingAspects)
        i += 1
      }
    }
    equals
  }
}

class NNPHead(form: String, originality: String) extends Head(form, Head.NNP, originality) {
  def init(config: NNPConfig, countability: String) {
    set(AspectName.ROLE, AspectValue.REG)
    set(AspectName.COUNTABILITY, countability)
    set(AspectName.GENRE, config.genre)
    set(AspectName.PERSON_NUMBER, config.personNumber)
    assign(AspectName.DET, Head.NODTHead)
    assign(AspectName.PREDET, Head.NOPDHead)
  }

  override def isPure: Boolean = super.isPure && read(AspectName.DET) == Head.NODTHead && read(AspectName.PREDET) == Head.NOPDHead

  override def shallowCopy(refPhrase: Phrase): NNPHead = {
    val peer = new NNPHead(this.form, this.originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): NNPHead = {
    val peer = new NNPHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }

  override def toString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
    s"$form[$typo,${get(AspectName.ROLE)},${get(AspectName.COUNTABILITY)},${get(AspectName.GENRE)}," +
    s"${get(AspectName.PERSON_NUMBER)},PREDET:${read(AspectName.PREDET)},DET:${read(AspectName.DET)}," +
    s"${preModifiers.mkString("m-[", ",", "]")},${postModifiers.mkString("m+[", ",", "]")}]"

  override def print(level: Int, prefix: String) {
    println(Head.getIndent(level, prefix) + s"${if (Head.SHOW_ID) s"($id)" else ""}" +
      s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
      s"$form[$typo,${get(AspectName.QW_MARK)},${get(AspectName.ROLE)},${get(AspectName.COUNTABILITY)}," +
      s"${get(AspectName.GENRE)},${get(AspectName.PERSON_NUMBER)}]" + s"${phrase.units.mkString("[", ",", "]")}")
    if (read(AspectName.PREDET).typo != Head.NOPD)
      read(AspectName.PREDET).print(level + 1, "PREDET")
    if (read(AspectName.DET).typo != Head.NODT)
      read(AspectName.DET).print(level + 1, "DET")
    var i = preModifiers.size - 1
    while (i >= 0) {
      preModifiers.apply(i).print(level + 1, "PREMOD")
      i -= 1
    }
    postModifiers.foreach(modifier => {
      modifier.print(level + 1, "POSTMOD")
    })
  }

  override def rewriteAspects(): Unit = {
    stringAspects.update(AspectName.COUNTABILITY, AspectValue.BICNT)
  }
}

class PNHead(form: String, originality: String) extends Head(form, Head.PN, originality) {
  def init(role: String, genre: String, personNumber: String, qw: String) {
    set(AspectName.ROLE, role)
    set(AspectName.GENRE, genre)
    set(AspectName.PERSON_NUMBER, personNumber)
    set(AspectName.QW_TYPE, qw)
    set(AspectName.QW_MARK, qw)
    assign(AspectName.REF, Head.UNDEFHead)
  }

  override def isPure: Boolean = super.isPure && read(AspectName.REF) == Head.UNDEFHead

  override def shallowCopy(refPhrase: Phrase): PNHead = {
    val peer = new PNHead(this.form, this.originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): PNHead = {
  //    val peer = new PNHead(this.form, this.originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): PNHead = {
    val peer = new PNHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }

  override def toString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
    s"$form[$typo,${get(AspectName.QW_MARK)},${get(AspectName.ROLE)},${get(AspectName.GENRE)}," +
    s"${get(AspectName.PERSON_NUMBER)},REF:${read(AspectName.REF)}]"
}

class VBPHead(form: String, originality: String) extends Head(form, Head.VBP, originality) {
  def init(config: VBPConfig, transitivity: String) {
    set(AspectName.IS_STEM, config.isStem)
    set(AspectName.TRANSITIVITY, transitivity)
    set(AspectName.FUNCTION, config.function)
    set(AspectName.VOICE, config.voice)
    set(AspectName.CONTINUANCE, config.continuance)
    set(AspectName.TENSE, config.tense)
    set(AspectName.PERSON_NUMBER, config.personNumber)
    assign(AspectName.MODALITY, Head.NOMDHead)
    assign(AspectName.POSITIVITY, config.positivity)
  }

  override def isPure: Boolean = super.isPure &&
    read(AspectName.MODALITY) == Head.NOMDHead &&
    (read(AspectName.POSITIVITY) == Head.POSHead || read(AspectName.POSITIVITY) == Head.UNDEFHead)

  override def shallowCopy(refPhrase: Phrase): VBPHead = {
    val peer = new VBPHead(form, originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): VBPHead = {
  //    val peer = new VBPHead(form, originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): VBPHead = {
    val peer = new VBPHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }

  override def toString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
    s"$form[$typo,${get(AspectName.IS_STEM)},${get(AspectName.TRANSITIVITY)}," +
    s"${get(AspectName.FUNCTION)},${get(AspectName.VOICE)},${get(AspectName.CONTINUANCE)}," +
    s"${get(AspectName.TENSE)},${get(AspectName.PERSON_NUMBER)}," +
    s"POSITIVITY=${read(AspectName.POSITIVITY)},MODALITY=${read(AspectName.MODALITY)}," +
    s"${variables.mkString("v[", ",", "]")},${preModifiers.mkString("m-[", ",", "]")}]"

  override def print(level: Int, prefix: String) {
    println(Head.getIndent(level, prefix) +
      s"${if (Head.SHOW_ID) s"($id)" else ""}" +
      s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
      s"$form[$typo,${get(AspectName.QW_MARK)},${get(AspectName.IS_STEM)},${get(AspectName.TRANSITIVITY)}," +
      s"${get(AspectName.FUNCTION)},${get(AspectName.VOICE)},${get(AspectName.CONTINUANCE)},${get(AspectName.TENSE)}," +
      s"${get(AspectName.PERSON_NUMBER)}],missing:${phrase.rule.getMissingSpecs.mkString("<", ",", ">")}" +
      phrase.units.mkString("[", ",", "]"))
    if (read(AspectName.POSITIVITY).typo != Head.POS && read(AspectName.POSITIVITY) != Head.UNDEFHead)
      read(AspectName.POSITIVITY).print(level + 1, "POSITIVITY")
    if (read(AspectName.MODALITY).typo != Head.NOMD)
      read(AspectName.MODALITY).print(level + 1, "MODALITY")
    variables.foreach(variable => {
      variable.print(level + 1, "VAR")
    })
    var i = preModifiers.size - 1
    while (i >= 0) {
      preModifiers.apply(i).print(level + 1, "PREMOD")
      i -= 1
    }
    postModifiers.foreach(modifier => {
      modifier.print(level + 1, "POSTMOD")
    })
    //    println(phrase.headMap)
    if (phrase.typo == Phrase.STRUCTURER)
      phrase.asInstanceOf[StructurerPhrase].candidateUpgraders.foreach(upgrader => {
        println(s"${upgrader.rule.isComplete}: ${upgrader.rule}")
      })
  }

  override def onOperationsPerformed(): Unit = {
    if (get(AspectName.FUNCTION) == AspectValue.PREDICATE) {
      if (get(AspectName.VOICE) == AspectValue.UNDEF)
        set(AspectName.VOICE, AspectValue.ACT)
      if (get(AspectName.CONTINUANCE) == AspectValue.UNDEF)
        set(AspectName.CONTINUANCE, AspectValue.GEN)
    }
  }
}

class VBMHead(form: String, originality: String) extends Head(form, Head.VBM, originality) {
  set(AspectName.IS_STEM, AspectValue.STEM)
  set(AspectName.FUNCTION, AspectValue.PREDICATE)
  set(AspectName.TENSE, AspectValue.PRES)
  set(AspectName.PERSON_NUMBER, AspectValue.ALL)
  assign(AspectName.POSITIVITY, Head.POSHead)

  override def isPure: Boolean = super.isPure && read(AspectName.POSITIVITY) == Head.POSHead

  override def shallowCopy(refPhrase: Phrase): VBMHead = {
    val peer = new VBMHead(form, originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): VBMHead = {
  //    val peer = new VBMHead(this.form, this.originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): VBMHead = {
    val peer = new VBMHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }
}

class VBAHead(form: String, originality: String) extends Head(form, Head.VBA, originality) {
  def init(isStem: String, function: String, tense: String, personNumber: String) {
    set(AspectName.IS_STEM, isStem)
    set(AspectName.FUNCTION, function)
    //    assign(AspectName.CONTINUANCE, if(function == AspectValue.PREDICATE) AspectValue.GEN else AspectValue.UNDEF)
    set(AspectName.TENSE, tense)
    set(AspectName.PERSON_NUMBER, personNumber)
    assign(AspectName.POSITIVITY, Head.POSHead)
  }

  override def isPure: Boolean = super.isPure && read(AspectName.POSITIVITY) == Head.POSHead

  override def shallowCopy(refPhrase: Phrase): VBAHead = {
    val peer = new VBAHead(form, originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): VBAHead = {
  //    val peer = new VBAHead(this.form, this.originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): VBAHead = {
    val peer = new VBAHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }
}

class AJEHead(form: String, originality: String) extends Head(form, Head.AJE, originality) {
  def init(degree: String) {
    set(AspectName.DEGREE, degree)
  }

  override def shallowCopy(refPhrase: Phrase): AJEHead = {
    val peer = new AJEHead(this.form, this.originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): AJEHead = {
  //    val peer = new AJEHead(this.form, this.originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): AJEHead = {
    val peer = new AJEHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }

  override def toString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
    s"$form[$typo,${get(AspectName.DEGREE)},${preModifiers.mkString("m-[", ",", "]")}]" +
    s"{${phrase.rule}}" + phrase.units.mkString("[", ",", "]")

  override def print(level: Int, prefix: String) {
    println(Head.getIndent(level, prefix) +
      s"${if (Head.SHOW_ID) s"($id)" else ""}" +
      s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
      s"$form[$typo,${get(AspectName.DEGREE)}]" + s"{${phrase.rule}}" + phrase.units.mkString("[", ",", "]"))
    var i = preModifiers.size - 1
    while (i >= 0) {
      preModifiers.apply(i).print(level + 1, "PREMOD")
      i -= 1
    }
    postModifiers.foreach(modifier => {
      modifier.print(level + 1, "POSTMOD")
    })
  }
}

class AVEHead(form: String, originality: String) extends Head(form, Head.AVE, originality) {
  def init(degree: String) {
    set(AspectName.DEGREE, degree)
  }

  override def shallowCopy(refPhrase: Phrase): AVEHead = {
    val peer = new AVEHead(this.form, this.originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): AVEHead = {
  //    val peer = new AVEHead(this.form, this.originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): AVEHead = {
    val peer = new AVEHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }

  override def toString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
    s"$form[$typo,${get(AspectName.DEGREE)},${preModifiers.mkString("m-[", ",", "]")}]"

  override def print(level: Int, prefix: String) {
    println(Head.getIndent(level, prefix) + s"${if (Head.SHOW_ID) s"($id)" else ""}" +
      s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
      s"$form[$typo,${get(AspectName.DEGREE)}]" + s"{${phrase.rule}}" + phrase.units.mkString("[", ",", "]"))
    var i = preModifiers.size - 1
    while (i >= 0) {
      preModifiers.apply(i).print(level + 1, "PREMOD")
      i -= 1
    }
    postModifiers.foreach(modifier => {
      modifier.print(level + 1, "POSTMOD")
    })
  }
}

class PPPHead(form: String, originality: String) extends Head(form, Head.PPP, originality) {
  override def shallowCopy(refPhrase: Phrase): PPPHead = {
    val peer = new PPPHead(this.form, this.originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): PPPHead = {
  //    val peer = new PPPHead(this.form, this.originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): PPPHead = {
    val peer = new PPPHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }

  override def toString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
    s"$form[$typo,${variables.mkString("v[", ",", "]")}]"

  override def print(level: Int, prefix: String) {
    println(Head.getIndent(level, prefix) + s"${if (Head.SHOW_ID) s"($id)" else ""}" +
      s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
      s"$form[$typo,${get(AspectName.QW_MARK)}],missing:${phrase.rule.getMissingSpecs.mkString("<", ",", ">")}" +
      phrase.units.mkString("[", ",", "]"))
    variables.foreach(variable => {
      variable.print(level + 1, "VAR")
    })
  }
}

class DECLHead(form: String, originality: String) extends Head(form, Head.DECL, originality) {
  set(AspectName.ROLE, AspectValue.REG)

  def init(coreHead: Head) {
    //    set(AspectName.FUNCTION, vbpHead.get(AspectName.FUNCTION))
    set(AspectName.VOICE, coreHead.get(AspectName.VOICE))
    set(AspectName.CONTINUANCE, coreHead.get(AspectName.CONTINUANCE))
    set(AspectName.TENSE, coreHead.get(AspectName.TENSE))
  }

  override def shallowCopy(refPhrase: Phrase): DECLHead = {
    val peer = new DECLHead(form, originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): DECLHead = {
  //    val peer = new DECLHead(this.form, this.originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): DECLHead = {
    val peer = new DECLHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }

  override def toString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
    s"$form[$typo,${get(AspectName.QW_MARK)},${get(AspectName.VOICE)},${get(AspectName.CONTINUANCE)}," +
    s"${get(AspectName.TENSE)},${variables.mkString("v[", ",", "]")}]"

  override def print(level: Int, prefix: String) {
    println(Head.getIndent(level, prefix) +
      s"${if (Head.SHOW_ID) s"($id)" else ""}" +
      s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
      s"$form[$typo,${get(AspectName.QW_MARK)},${get(AspectName.VOICE)}," +
      s"${get(AspectName.CONTINUANCE)},${get(AspectName.TENSE)}]")
    //    read(AspectName.SUBJ).print(level + 1, "SUBJ")
    //    read(AspectName.PRED).print(level + 1, "PRED")
    variables.foreach(variable => {
      variable.print(level + 1, "VAR")
    })
    var i = preModifiers.size - 1
    while (i >= 0) {
      preModifiers.apply(i).print(level + 1, "PREMOD")
      i -= 1
    }
    postModifiers.foreach(modifier => {
      modifier.print(level + 1, "POSTMOD")
    })
  }
}

class INYNHead(form: String, originality: String) extends Head(form, Head.INYN, originality) {
  def init(coreHead: Head) {
    set(AspectName.VOICE, coreHead.get(AspectName.VOICE))
    set(AspectName.CONTINUANCE, coreHead.get(AspectName.CONTINUANCE))
    set(AspectName.TENSE, coreHead.get(AspectName.TENSE))
  }

  override def shallowCopy(refPhrase: Phrase): INYNHead = {
    val peer = new INYNHead(form, originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): INYNHead = {
  //    val peer = new INYNHead(this.form, this.originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): INYNHead = {
    val peer = new INYNHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }

  override def toString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
    s"$form[$typo,${get(AspectName.VOICE)},${get(AspectName.CONTINUANCE)},${get(AspectName.TENSE)}," +
    s"${variables.mkString("v[", ",", "]")}]"

  override def print(level: Int, prefix: String) {
    println(Head.getIndent(level, prefix) +
      s"${if (Head.SHOW_ID) s"($id)" else ""}" +
      s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
      s"$form[$typo,${get(AspectName.VOICE)},${get(AspectName.CONTINUANCE)},${get(AspectName.TENSE)}]," +
      s"missing:${phrase.rule.getMissingSpecs.mkString("<", ",", ">")}")
    variables.foreach(variable => {
      variable.print(level + 1, "VAR")
    })
    var i = preModifiers.size - 1
    while (i >= 0) {
      preModifiers.apply(i).print(level + 1, "PREMOD")
      i -= 1
    }
    postModifiers.foreach(modifier => {
      modifier.print(level + 1, "POSTMOD")
    })
  }
}

class INWHHead(form: String, originality: String) extends Head(form, Head.INWH, originality) {
  assign(AspectName.TARGET, Head.NOTARGETHead)

  def init(coreHead: Head) {
    set(AspectName.VOICE, coreHead.get(AspectName.VOICE))
    set(AspectName.CONTINUANCE, coreHead.get(AspectName.CONTINUANCE))
    set(AspectName.TENSE, coreHead.get(AspectName.TENSE))
  }

  override def shallowCopy(refPhrase: Phrase): INWHHead = {
    val peer = new INWHHead(form, originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): INWHHead = {
  //    val peer = new INWHHead(this.form, this.originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): INWHHead = {
    val peer = new INWHHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }

  override def toString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
    s"$form[$typo,${get(AspectName.QW_MARK)},${get(AspectName.VOICE)},${get(AspectName.CONTINUANCE)}," +
    s"${get(AspectName.TENSE)},${variables.mkString("v[", ",", "]")}]"

  override def print(level: Int, prefix: String) {
    println(Head.getIndent(level, prefix) +
      s"${if (Head.SHOW_ID) s"($id)" else ""}" +
      s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
      s"$form[$typo,${get(AspectName.QW_MARK)},${get(AspectName.VOICE)},${get(AspectName.CONTINUANCE)}," +
      s"${get(AspectName.TENSE)}],missing:${phrase.rule.getMissingSpecs.mkString("<", ",", ">")}")
    //    if(read(AspectName.TARGET).typo != Head.NOTARGET)
    //      read(AspectName.TARGET).print(level + 1, "TARGET")
    variables.foreach(variable => {
      variable.print(level + 1, "VAR")
    })
    var i = preModifiers.size - 1
    while (i >= 0) {
      preModifiers.apply(i).print(level + 1, "PREMOD")
      i -= 1
    }
    postModifiers.foreach(modifier => {
      modifier.print(level + 1, "POSTMOD")
    })
  }
}

class CLAUSEHead(form: String, originality: String) extends Head(form, Head.CLAUSE, originality) {
  assign(AspectName.TARGET, Head.NOTARGETHead)
  set(AspectName.ROLE, AspectValue.REG)

  def init(coreHead: Head) {
    set(AspectName.VOICE, coreHead.get(AspectName.VOICE))
    set(AspectName.CONTINUANCE, coreHead.get(AspectName.CONTINUANCE))
    set(AspectName.TENSE, coreHead.get(AspectName.TENSE))
  }

  override def shallowCopy(refPhrase: Phrase): CLAUSEHead = {
    val peer = new CLAUSEHead(form, originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): CLAUSEHead = {
  //    val peer = new CLAUSEHead(this.form, this.originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): CLAUSEHead = {
    val peer = new CLAUSEHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }

  override def toString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
    s"$form[$typo,${get(AspectName.QW_MARK)},${get(AspectName.ROLE)},${get(AspectName.VOICE)}," +
    s"${get(AspectName.CONTINUANCE)},${get(AspectName.TENSE)},${variables.mkString("v[", ",", "]")}]"

  override def print(level: Int, prefix: String) {
    println(Head.getIndent(level, prefix) +
      s"${if (Head.SHOW_ID) s"($id)" else ""}" +
      s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
      s"$form[$typo,${get(AspectName.QW_MARK)},${get(AspectName.ROLE)},${get(AspectName.VOICE)}," +
      s"${get(AspectName.CONTINUANCE)},${get(AspectName.TENSE)}]" + s"${phrase.units.mkString("[", ",", "]")}")
    //    if(read(AspectName.TARGET).typo != Head.NOTARGET)
    //      read(AspectName.TARGET).print(level + 1, "TARGET")
    variables.foreach(variable => {
      variable.print(level + 1, "VAR")
    })
    var i = preModifiers.size - 1
    while (i >= 0) {
      preModifiers.apply(i).print(level + 1, "PREMOD")
      i -= 1
    }
    postModifiers.foreach(modifier => {
      modifier.print(level + 1, "POSTMOD")
    })
  }
}

class CJPHead(form: String, originality: String) extends Head(form, Head.CJP, originality) {

  override def toString: String = s"${if (Head.SHOW_ID) s"($id)" else ""}" +
    s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
    s"$form[$typo,${get(AspectName.QW_MARK)}]"

  override def print(level: Int, prefix: String) {
    println(Head.getIndent(level, prefix) +
      s"${if (Head.SHOW_ID) s"($id)" else ""}" +
      s"${if (Phrase.SHOW_ID && phrase != null) s"{${phrase.id}}" else ""}" +
      s"$form[$typo,${get(AspectName.QW_MARK)}],missing:${phrase.rule.getMissingSpecs.mkString("<", ",", ">")}" +
      s"${phrase.units.mkString("[", ",", "]")}")
    variables.foreach(variable => {
      variable.print(level + 1, "VAR")
    })
  }

  override def shallowCopy(refPhrase: Phrase): CJPHead = {
    val peer = new CJPHead(form, originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): CJPHead = {
  //    val peer = new CJPHead(this.form, this.originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): CJPHead = {
    val peer = new CJPHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }

  /**
    * TODO: this function could check the aspects of the pair in order to adjust the quality of this.
    */
  def assimilateAspects(varTypo: String, headA: Head, headB: Head) {
    def assimilateAspect(aspectName: String, defaultValue: String): String = {
      if (headA.get(aspectName) == headB.get(aspectName) && headB.get(aspectName) != null)
        headB.get(aspectName)
      else
        defaultValue
    }

    set(AspectName.VAR_TYPO, varTypo)
    if (varTypo == Head.NNP || varTypo == Head.PN || varTypo == Head.NNS || varTypo == Head.NNL) {
      set(AspectName.ROLE, assimilateAspect(AspectName.ROLE, AspectValue.REG))
      set(AspectName.GENRE, assimilateAspect(AspectName.GENRE, AspectValue.SBSTH))
      set(AspectName.PERSON_NUMBER, AspectValue.PL)
    }
    else if (varTypo == Head.VBP) {
      set(AspectName.IS_STEM, assimilateAspect(AspectName.IS_STEM, AspectValue.UNDEF))
      set(AspectName.FUNCTION, assimilateAspect(AspectName.FUNCTION, AspectValue.UNDEF))
      set(AspectName.VOICE, assimilateAspect(AspectName.VOICE, AspectValue.UNDEF))
      set(AspectName.CONTINUANCE, assimilateAspect(AspectName.CONTINUANCE, AspectValue.UNDEF))
      set(AspectName.TENSE, assimilateAspect(AspectName.TENSE, AspectValue.UNDEF))
      set(AspectName.PERSON_NUMBER, assimilateAspect(AspectName.PERSON_NUMBER, AspectValue.UNDEF))
    }
    else if (varTypo == Head.VBM) {
      set(AspectName.IS_STEM, assimilateAspect(AspectName.IS_STEM, AspectValue.UNDEF))
      set(AspectName.FUNCTION, assimilateAspect(AspectName.FUNCTION, AspectValue.UNDEF))
      set(AspectName.VOICE, assimilateAspect(AspectName.VOICE, AspectValue.UNDEF))
      set(AspectName.TENSE, assimilateAspect(AspectName.TENSE, AspectValue.UNDEF))
      set(AspectName.PERSON_NUMBER, assimilateAspect(AspectName.PERSON_NUMBER, AspectValue.UNDEF))
    }
    else if (varTypo == Head.VBA) {
      set(AspectName.IS_STEM, assimilateAspect(AspectName.IS_STEM, AspectValue.UNDEF))
      set(AspectName.FUNCTION, assimilateAspect(AspectName.FUNCTION, AspectValue.UNDEF))
      set(AspectName.VOICE, assimilateAspect(AspectName.VOICE, AspectValue.UNDEF))
      set(AspectName.TENSE, assimilateAspect(AspectName.TENSE, AspectValue.UNDEF))
      set(AspectName.PERSON_NUMBER, assimilateAspect(AspectName.PERSON_NUMBER, AspectValue.UNDEF))
    }
    else {
      headB.stringAspects.foreach(pair => {
        set(pair._1, assimilateAspect(pair._1, AspectValue.UNDEF))
      })
    }
  }

  /**
    * TODO: the definition of the conciliation of two rules is too strong
    */
  def generateHeadRule(headA: Head, headB: Head): Rule = {
    var found = false
    if (headA.phrase.isPrime && headB.phrase.isPrime) {
      val ruleA = headA.phrase.rule
      val ruleB = headB.phrase.rule
      if (ruleA.body.size == ruleB.body.size) {
        var i = 0
        var contradicted = false
        while (i < ruleA.fittedVector.size && !contradicted) {
          if (ruleA.fittedVector.apply(i) == ruleA.fittedVector.apply(i)) {
            if (!ruleA.fittedVector.apply(i))
              contradicted = ruleA.body.apply(i).toString != ruleB.body.apply(i).toString
          }
          else
            contradicted = true
          i += 1
        }
        contradicted = contradicted && ruleA.product.toString != ruleB.product.toString
        found = !contradicted
      }
    }
    if (found)
      headB.phrase.rule.copy
    else
      null
  }
}

class COMPHead(form: String, originality: String) extends Head(form, Head.COMP, originality) {
  override def shallowCopy(refPhrase: Phrase): COMPHead = {
    val peer = new COMPHead(form, originality)
    peer.phrase = refPhrase
    peer.performShallowCopy(this)
    peer
  }

  //  override def deepCopy(map: mutable.HashMap[String, Head]): COMPHead = {
  //    val peer = new COMPHead(this.form, this.originality)
  //    peer.performDeepCopy(this, map)
  //    peer
  //  }

  override def deepCopy(peerHeadMap: mutable.HashMap[String, Head], peerPhraseMap: mutable.HashMap[Int, Phrase]): COMPHead = {
    val peer = new COMPHead(this.form, this.originality)
    peer.performDeepCopy(this, peerHeadMap, peerPhraseMap)
    peer
  }
}
