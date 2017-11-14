package com.deameamo.muse.longman.analysis

import scala.collection.mutable.MutableList
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

object PhraseFactory {
  var currId = 1
  
  def create(rule: Rule): Phrase = {
    val phrase = {
      rule.category match {
        case Rule.GNR => new StructurerPhrase(rule)
        case Rule.LEX => new StructurerPhrase(rule)
        case Rule.SEL => new UpgraderPhrase(rule)
        case Rule.ALT => new UpgraderPhrase(rule)
        case Rule.MWE => new MWEPhrase(rule)
        case _ => null
      }
    }
    phrase.id = currId
    currId += 1
    phrase
  }
  
  val upgraders = new MutableList[Phrase]
  
  def addUpgraders(cell: Cell) {
    val selectedList = new MutableList[Phrase]
    cell.phrases.foreach(cellPhrase => {
      if(cellPhrase.isInstanceOf[StructurerPhrase]) {
        val cellPhraseHead = {
          if(cellPhrase.isPrime)
            cellPhrase.initHead
          else if(cellPhrase.isDone && ((cellPhrase.isCorePhrase && cellPhrase.rule.body.size == 1) || !cellPhrase.isCorePhrase))
            cellPhrase.coreHead
          else
            null
        }
        if(cellPhraseHead != null) {
          upgraders.foreach(upgrader => {
            if(RuleEngine.fit(cellPhraseHead, upgrader.initSpec)) {
              val upgraderPeer = upgrader.deepCopy
//              upgraderPeer.rule.headIdVector.update(upgraderPeer.initHeadIndex, cellPhraseHead.id)
              selectedList += upgraderPeer
//      println(s"selectedList: upgrader.initSpec=${upgrader.initSpec}, cellPhraseHead=${cellPhraseHead}")
            }
          })
        }
      }
    })
    cell.phrases ++= selectedList
  }
}

object Phrase {
  val STRUCTURER = "STRUCTURER"
  val UPGRADER = "UPGRADER"
  val MWE = "MWE"
  
  var count = 0
  
  var SHOW_ID = true
  
  val emptyUnits = new MutableList[EntryUnit]
}

abstract class Phrase(var rule: Rule) {
  var id = 0
  
  val typo: String
  
  var coreHead: Head = null
  
  var isCorePhrase = false
  
  val units = new HashSet[EntryUnit]
  
  var sortedUnits: MutableList[EntryUnit] = null
  
  val headMap = new HashMap[String, Head]
  
  var initHead: Head = null
  
  var initSpec: Spec = null
  
  var initHeadIndex = -1
  
  var bagOfHwds: HashSet[String] = null
  
  val peerHeadMap = new HashMap[String, Head]
  
  val peerPhraseMap = new HashMap[Int, Phrase]
  
  var extraPoint: Int = 0
  
  def init(unit: EntryUnit, _initHead: Head, _bagOfHwds: HashSet[String]): Boolean = {
    initHead = _initHead.shallowCopy(this)
    bagOfHwds = _bagOfHwds
    var i = 0
    var succeeds = true
    if(rule.body.size > 1) {
      while(i < rule.body.size && succeeds) {
        val spec = rule.body.apply(i)
        if(!spec.forms.isEmpty) {
          succeeds = false
          var j = 0
          while(j < spec.forms.size && !succeeds) {
            succeeds = bagOfHwds.contains(spec.forms.apply(j))
            j += 1
          }
          if(spec.forms.contains(initHead.form))
            succeeds = succeeds && (if(spec.typo != Head.UNDEF) spec.typo == initHead.typo else true)
        }
        i += 1
      }
    }
    else {
      val spec = rule.body.head
      succeeds = (if(spec.typo != Head.UNDEF) spec.typo == initHead.typo else true)
    }
    if(succeeds) {
      if(unit.gloss == null && unit.isInstanceOf[Exa]) {
        units += unit.asInstanceOf[Exa].parent
      }
      else
        units += unit
      initHead.phrase = this
      initHeadIndex = getInitHeadIndex(initHead)
      rule.from = initHeadIndex
      rule.to = initHeadIndex
//      initSpec = rule.body.apply(initHeadIndex)
      initSpec = rule.fit(initHead, Rule.FORWARD)
      initSpec.typo = initHead.typo
      if(initSpec.id != null)
        headMap.put(initSpec.id, initHead)
      rule.updateStatus
      if(initSpec.isCore) {
        coreHead = initHead
        isCorePhrase = true
        coreHead.quality += rule.numberOfMissingSpecs * Rule.MIS_PT
      }
    }
    succeeds
  }
  
  def updateInitHeadId {
    if(isCorePhrase)
      coreHead.id = Head.getCoreHeadId
    else
      initHead.id = Head.getInitHeadId
  }
  
  def deepCopy: Phrase = {
//    if(coreHead != null)
//      coreHead.print
    peerHeadMap.clear
    peerPhraseMap.clear
    if(coreHead != null) {
      val coreHeadPeer = coreHead.deepCopy(peerHeadMap, peerPhraseMap)
      if(this.initHead != null && !peerHeadMap.contains(this.initHead.id)) {
        val initHeadPeer = initHead.deepCopy(peerHeadMap, peerPhraseMap)
        initHeadPeer.replaceHead(peerHeadMap)
      }
      coreHeadPeer.replaceHead(peerHeadMap)
    }
    else {
      val initHeadPeer = initHead.deepCopy(peerHeadMap, peerPhraseMap)
      initHeadPeer.replaceHead(peerHeadMap)
    }
    peerHeadMap.foreach(pair => {
      Debugger.println(s"head peer: ${pair}")
    })
    peerPhraseMap.foreach(pair => {
      Debugger.println(s"phrase pair: ${pair}")
    })
    peerPhraseMap.foreach(pair => {
      val peerPhrase = pair._2
      if(peerPhrase.initHead != null) {
        val peerInitHead = peerHeadMap.apply(peerPhrase.initHead.id)
        if(peerPhrase.initHead != peerInitHead)
          peerPhrase.initHead = peerInitHead
      }
      if(peerPhrase.coreHead != null) {
        val peerCoreHead = peerHeadMap.apply(peerPhrase.coreHead.id)
        if(peerPhrase.coreHead != peerCoreHead)
          peerPhrase.coreHead = peerCoreHead
      }
    })
    peerPhraseMap.apply(id)
  }
  
  def shallowCopy: Phrase
  
  def sortUnits {
    if(units.size > 0) {
      val temp = new MutableList[EntryUnit]
      temp ++= units
      sortedUnits = temp.sortWith((a, b)=> {
        a.index.compareTo(b.index) > 0
      })
      println(sortedUnits)
    }
    else {
      sortedUnits = Phrase.emptyUnits
    }
  }
  
  protected def getInitHeadIndex(initHead: Head): Int = {
    var index = -1
    var i = 0
    while(i < rule.body.size && index == -1) {
      if(rule.body.apply(i).forms.contains(initHead.form))
        index = i
      i += 1
    }
    index
  }
  
  override def toString = s"Phrase{${id}}: ${if(coreHead != null) s"head(c):${coreHead}" else s"head(i):${initHead}"}, rule:${rule}, units:${units.mkString("[", ",", "]")}"
  
  def equalsTo(another: Phrase) = this.initHead.equalsTo(another.initHead) && this.rule.string == another.rule.string
  
  def isPrime = rule.status == Rule.PRIME
  
  def isDone = rule.status == Rule.DONE
  
  def absolve(anotherPhrase: Phrase, direction: String): Phrase
}

class StructurerPhrase(_rule: Rule) extends Phrase(_rule) { absolving => 
  val typo = Phrase.STRUCTURER
  
  val candidateUpgraders = new MutableList[UpgraderPhrase]
  
  def upgradeByUpgraders(upgraders: MutableList[UpgraderPhrase]): MutableList[Phrase] = {
    val upgradedList = new MutableList[Phrase]
    var maxQuality = -1
    upgraders.foreach(upgrader => {
      val upgraded = upgrader.upgrade(this)
      if(upgraded != null) {
        maxQuality = Math.max(maxQuality, upgraded.coreHead.quality)
        upgradedList += upgraded
      }
    })
    if(upgradedList.isEmpty)
      upgradedList += this
    else {
      val siftedList = new MutableList[Phrase]
      upgradedList.foreach(upgraded => {
        if(upgraded.coreHead.quality == maxQuality)
          siftedList += upgraded
      })
      siftedList
    }
  }
  
//  def upgradeByCandidateUpgraders {
//    candidateUpgraders.foreach(upgrader => {
//      if(upgrader.isUpgradable(this)) {
//        upgrader.upgrade(this)
//      }
//    })
//  }
  
  def upgradeByCandidateUpgraders(wholePhrase: Phrase, list: MutableList[Phrase]) {
    val upgradedList = new MutableList[Phrase]
    var maxQuality = -1
    candidateUpgraders.foreach(upgrader => {
      println(s"upgradeByCandidateUpgraders before: this.isDone=${this.isDone}, this.rule=${this.rule}, upgrader=${upgrader.rule}")
      val upgradedWholePhrase = upgrader.upgradeWholePhrase(this, wholePhrase)
      println(s"upgradeByCandidateUpgraders after: upgrader=${upgrader.rule}, upgradedWholePhrase=${upgradedWholePhrase}")
      if(upgradedWholePhrase != null) {
        maxQuality = Math.max(maxQuality, upgradedWholePhrase.coreHead.quality)
        upgradedList += upgradedWholePhrase
      }
    })
    if(!upgradedList.isEmpty) {
      upgradedList.foreach(upgradedWholePhrase => {
        if(upgradedWholePhrase.coreHead.quality == maxQuality) {
          list += upgradedWholePhrase
        }
      })
    }
  }
  
  def addCandidateUpgraders(upgraders: MutableList[UpgraderPhrase]) {
  
    def inCandidateUpgraders(upgrader: Phrase): Boolean = {
      var in = false
      var i = 0
      while(i < candidateUpgraders.size && !in) {
        in = candidateUpgraders.apply(i).rule.string == upgrader.rule.string
        i += 1
      }
      in
    }
    
    upgraders.foreach(upgrader => {
      if(this.initHead != null) {
        if(this.initHead.fitsWith(upgrader.initHead) &&
            (if(this.coreHead != null && upgrader.coreHead != null) this.coreHead.fitsWith(upgrader.coreHead) else true) &&
            !inCandidateUpgraders(upgrader))
          candidateUpgraders += upgrader.deepCopy.asInstanceOf[UpgraderPhrase]
      }
    })
//    if(candidateUpgraders.size > 0)
//    println(s"addCandidateUpgraders: ")
//    candidateUpgraders.foreach(println(_))
  }
  
//  def upgradeChildren: Int = {
//    var gain = 0
//    if(coreHead != null) {
//      val originalQuality = coreHead.quality
//      upgradeByCandidateUpgraders
//      gain = coreHead.quality - originalQuality
//      coreHead.variables.foreach(variable => {
//        if(variable.phrase.typo == Phrase.STRUCTURER) {
//          gain += variable.phrase.asInstanceOf[StructurerPhrase].upgradeChildren
//        }
//      })
//      coreHead.preModifiers.foreach(modifier => {
//        if(modifier.phrase.typo == Phrase.STRUCTURER) {
//          gain += modifier.phrase.asInstanceOf[StructurerPhrase].upgradeChildren
//        }
//      })
//      coreHead.postModifiers.foreach(modifier => {
//        if(modifier.phrase.typo == Phrase.STRUCTURER) {
//          gain += modifier.phrase.asInstanceOf[StructurerPhrase].upgradeChildren
//        }
//      })
//    }
//    gain
//  }
  
  def upgradeChildren(wholePhrase: Phrase, list: MutableList[Phrase]) {
    if(coreHead != null) {
      upgradeByCandidateUpgraders(wholePhrase, list)
      coreHead.variables.foreach(variable => {
        if(variable.phrase.typo == Phrase.STRUCTURER) {
          variable.phrase.asInstanceOf[StructurerPhrase].upgradeChildren(wholePhrase, list)
        }
      })
      coreHead.preModifiers.foreach(modifier => {
        if(modifier.phrase.typo == Phrase.STRUCTURER) {
          modifier.phrase.asInstanceOf[StructurerPhrase].upgradeChildren(wholePhrase, list)
        }
      })
      coreHead.postModifiers.foreach(modifier => {
        if(modifier.phrase.typo == Phrase.STRUCTURER) {
          modifier.phrase.asInstanceOf[StructurerPhrase].upgradeChildren(wholePhrase, list)
        }
      })
    }
  }
  
  def performOperations(operations: MutableList[Operation], operationHeadMap: HashMap[String, Head]): MutableList[Phrase] = {
    val list = new MutableList[Phrase]
    if(!operations.isEmpty) {
      var hasInserOperation = false
      operations.foreach(operation => {
        operation.operator match {
          case Operator.INSERT => {
            hasInserOperation = true
            if(operation.operands.size == 1)
              list ++= insertGenerally(operation.operands, operationHeadMap)
            else if(operation.operands.size == 2)
              list ++= insertSpecifically(operation.operands, operationHeadMap)
              
          }
          case _ =>
        }
      })
      if(!hasInserOperation) {
        list += this
      }
    }
    else {
      list += this
    }
    list
  }
  
  // insert
  def insertGenerally(operands: MutableList[String], operationHeadMap: HashMap[String, Head]): MutableList[Phrase] = {
    val list = new MutableList[Phrase]
    val questionHead = operationHeadMap.apply(operands.apply(0))
    Debugger.debug(x => {
      println(s"insertGenerally: questionHead=${questionHead.toSimpleString}, insertee=${this.coreHead.toSimpleString}")
//      questionHead.print
//      println(s"insert: insertee=${this.coreHead.toSimpleString}")
//      this.coreHead.print
    })
    coreHead.insertGenerally(questionHead, this, list, null)
    list
  }
  
  def insertSpecifically(operands: MutableList[String], operationHeadMap: HashMap[String, Head]): MutableList[Phrase] = {
    val list = new MutableList[Phrase]
    val inserter = operationHeadMap.apply(operands.apply(0))
    val insertee = operationHeadMap.apply(operands.apply(1))
//    println(s"insertInHead: inserter=${inserter.toSimpleString}, insertee=${insertee}")
    if(insertee.phrase.typo == Phrase.STRUCTURER) {
      val index = insertee.phrase.rule.getMissingSpecIndex(inserter)
      if(index != -1) {
        val wholePhrasePeer = this.deepCopy
        val inserteePeer = this.peerHeadMap.apply(insertee.id)
        println(s"insertSpecifically before: inserteePeer.phrase.isDone=${inserteePeer.phrase.isDone}, inserteePeer.phrase.rule=${inserteePeer.phrase.rule}")
        var gain = inserteePeer.phrase.asInstanceOf[StructurerPhrase].insertAsMissing(inserter, index)
        println(s"insertSpecifically after : inserteePeer.phrase.isDone=${inserteePeer.phrase.isDone}, inserteePeer.phrase.rule=${inserteePeer.phrase.rule}")
        wholePhrasePeer.coreHead.quality += gain
//        list += wholePhrasePeer
//        insertee.phrase.asInstanceOf[StructurerPhrase].candidateUpgraders.foreach(println(_))
        val originalListSize = list.size
        inserteePeer.phrase.asInstanceOf[StructurerPhrase].upgradeByCandidateUpgraders(wholePhrasePeer, list)
        if(list.size == originalListSize)
          list += wholePhrasePeer
      }
    }
    list
  }
  
  def insertAsMissing(inserter: Head, specIndex: Int): Int = {
//    if(candidateUpgraders.size > 0)
//      println(s"insertAsMissing: upgraders")
//    candidateUpgraders.foreach(println(_))
    val originalQuality = coreHead.quality
    val spec = rule.body.apply(specIndex)
    rule.fittedVector.update(specIndex, true)
    if(spec.id != null) {
      headMap.put(spec.id, inserter)
    }
    if(!spec.operations.isEmpty) {
      inserter.performOperations(spec.operations, headMap)
    }
    coreHead.quality -= Rule.MIS_PT
    if(rule.isComplete) {
      var resultPhrase = rule.createProductPhrase(this)
      if(resultPhrase != null) {
        resultPhrase.rule.status = Rule.DONE
        // perform operations on the product
        if(!resultPhrase.rule.product.operations.isEmpty) {
          resultPhrase.coreHead.performOperations(resultPhrase.rule.product.operations, headMap)
        }
        resultPhrase.coreHead.quality += Rule.getPoint(resultPhrase.rule) * (resultPhrase.rule.body.size - 1) + resultPhrase.extraPoint
      }
      resultPhrase.coreHead.quality - originalQuality
    }
    coreHead.quality - originalQuality
  }
  
  def insertByInsertionRule(inserter: Head, insertionRule: InsertionRule): Int = {
    val originalQuality = coreHead.quality
    insertionRule.produce(this, inserter)
    coreHead.quality - originalQuality
  }
  
  def absolve(absolved: Phrase, direction: String): Phrase = {
    if(!absolved.isInstanceOf[StructurerPhrase])
      return null
    var resultPhrase: Phrase = null
    if(absolving.isCorePhrase && absolved.coreHead != null) {
      if(absolving.coreHead.typo == Head.CJP) {
        val specs = new MutableList[Spec]
        var passed = true
        var i = 0
        while(i < absolving.coreHead.variables.size && passed) {
          val variable = absolving.coreHead.variables.apply(i)
          val spec = variable.phrase.rule.fit(absolved.coreHead, direction)
          if(spec == null)
            passed = false
          else
            specs += spec
          i += 1
        }
        if(passed) {
          val absolvingPeer = absolving.deepCopy
          var i = 0
          var failed = false
          var originalVariableQuality = -1
          while(i < absolvingPeer.coreHead.variables.size && !failed) {
            val variable = absolvingPeer.coreHead.variables.apply(i)
            val absolvedPeer = absolved.deepCopy
            originalVariableQuality = variable.phrase.coreHead.quality
            val variableResultPhrase = variable.phrase.absolve(absolvedPeer, direction)
            if(variableResultPhrase != null) {
              absolvingPeer.coreHead.quality += (variableResultPhrase.coreHead.quality - originalVariableQuality)
              if(variableResultPhrase != variable.phrase) {
                absolvingPeer.coreHead.variables.update(i, variableResultPhrase.coreHead)
              }
            }
            else
              failed = true
            i += 1
          }
          if(!failed)
            resultPhrase = absolvingPeer
        }
      }
      else {
        val spec = absolving.rule.fit(absolved.coreHead, direction)
        if(spec != null) {
          val absolvingPeer = absolving.deepCopy.asInstanceOf[StructurerPhrase]
          val absolvedPeer = absolved.deepCopy
          resultPhrase = absolvingPeer.absolveByCorePhrase(spec, absolvedPeer, absolvedPeer.coreHead, direction)
        }
      }
    }
    else if(absolving.isCorePhrase && absolved.coreHead == null && absolved.isPrime && (absolving.coreHead.typo == Head.CJ || absolving.coreHead.typo == Head.CJP)) {
      val spec = absolving.rule.fit(absolved.initHead, direction)
      if(spec != null) {
        val absolvingPeer = absolving.deepCopy.asInstanceOf[StructurerPhrase]
        val absolvedPeer = absolved.deepCopy
        resultPhrase = absolvingPeer.absolveByCorePhrase(spec, absolvedPeer, absolvedPeer.initHead, direction)
      }
    }
    else if(!absolving.isCorePhrase && absolved.coreHead != null) {
//      Debugger.println(s"absolve, case 3 begin")
      val spec = absolving.rule.fit(absolved.coreHead, direction)
//      println(s"absolve, case 3: absolving.rule=${absolving.rule}, absolved.coreHead=${absolved.coreHead}, spec=${spec}, varTypo=${absolved.coreHead.get(AspectName.VAR_TYPO)}")
      if(spec != null) {
//        Debugger.println(s"absolve, case 3 begin: absolving.rule=${absolving.rule}, absolved.coreHead=${absolved.coreHead}")
        val absolvingPeer = absolving.deepCopy.asInstanceOf[StructurerPhrase]
        val absolvedPeer = absolved.deepCopy
//        Debugger.println(s"absolve, case 3 before absolving:")
        resultPhrase = absolvingPeer.absolveByNonCorePhrase(spec, absolvedPeer, absolvedPeer.coreHead, direction)
      }
    }
    else if(!absolving.isCorePhrase && absolved.coreHead == null && absolved.isPrime) {
      val spec = absolving.rule.fit(absolved.initHead, direction)
      if(spec != null) {
        val absolvingPeer = absolving.deepCopy.asInstanceOf[StructurerPhrase]
        val absolvedPeer = absolved.deepCopy
        Debugger.println(s"absolve, case 4 begin")
        resultPhrase = absolvingPeer.absolveByNonCorePhrase(spec, absolvedPeer, absolvedPeer.initHead, direction)
        if(absolved.rule.fitSpec(absolving.rule.getSpec(direction, 0), Rule.reverseDirection(direction))) {
          if(resultPhrase.coreHead != null) {
            resultPhrase.coreHead.quality -= Rule.MIS_PT
          }
          else {
            resultPhrase.extraPoint -= Rule.MIS_PT
          }
        }
      }
    }
    resultPhrase
  }
  
  private def absolveByCorePhrase(spec: Spec, anotherPhrase: Phrase, incomingHead: Head, direction: String): Phrase = {
    if(spec.id != null) {
      headMap.put(spec.id, incomingHead)
    }
    if(!spec.operations.isEmpty) {
      incomingHead.performOperations(spec.operations, headMap)
    }
    rule.updateStatus
    coreHead.quality -= Rule.MIS_PT
    if(rule.status == Rule.DONE) {
      var resultPhrase = rule.createProductPhrase(this)
      if(resultPhrase != null) {
        // perform operations on the product
        if(resultPhrase.rule.isComplete) {
          if(!resultPhrase.rule.product.operations.isEmpty) {
            val successful = resultPhrase.coreHead.performOperations(resultPhrase.rule.product.operations, headMap)
            if(successful == false)
              return null
          }
//          if(resultPhrase.coreHead != null)
//            resultPhrase.coreHead.quality += Rule.getPoint(rule) * (rule.body.size - 1) + resultPhrase.extraPoint
//          else
//            resultPhrase.initHead.quality += Rule.getPoint(rule) * (rule.body.size - 1) + resultPhrase.extraPoint
          if(resultPhrase.coreHead != null)
            resultPhrase.coreHead.quality += Rule.getPoint(rule) + resultPhrase.extraPoint
          else
            resultPhrase.initHead.quality += Rule.getPoint(rule) + resultPhrase.extraPoint
        }
      }
      resultPhrase
    }
    else
      this
  }
  
  private def absolveByNonCorePhrase(spec: Spec, absolved: Phrase, incomingHead: Head, direction: String): Phrase = {
    if(spec.isCore) {
      coreHead = incomingHead
      coreHead.quality += rule.numberOfMissingSpecs * Rule.MIS_PT
    }
    if(spec.id != null) {
      headMap.put(spec.id, incomingHead)
    }
    if(!spec.operations.isEmpty) {
      incomingHead.performOperationsInComplexHead(spec.operations, headMap)
    }
    rule.updateStatus
    if(coreHead != null) {
      coreHead.quality -= Rule.MIS_PT
    }
    if(rule.status == Rule.DONE) {
      var resultPhrase = rule.createProductPhrase(this)
      if(resultPhrase != null) {
        // perform operations on the product
        if(resultPhrase.rule.isComplete) {
          if(!resultPhrase.rule.product.operations.isEmpty) {
            resultPhrase.coreHead.performOperationsInComplexHead(resultPhrase.rule.product.operations, headMap)
          }
//          println(s"absolveByNonCorePhrase, before: resultPhrase.coreHead.quality=${resultPhrase.coreHead.quality}")
//          resultPhrase.coreHead.quality += Rule.getPoint(rule) * (rule.body.size - 1) + resultPhrase.extraPoint
          resultPhrase.coreHead.quality += Rule.getPoint(rule) + resultPhrase.extraPoint
//          println(s"absolveByNonCorePhrase, before: resultPhrase.coreHead.quality=${resultPhrase.coreHead.quality}")
          if(!resultPhrase.isCorePhrase && !resultPhrase.coreHead.phrase.isCorePhrase) {
            resultPhrase = resultPhrase.coreHead.phrase
          }
        }
      }
      resultPhrase
    }
    else
      this
  }
  
  def shallowCopy: Phrase = {
    val peer = new StructurerPhrase(rule.copy)
    peer.id = this.id
    peer.initHead = this.initHead
    peer.initSpec = this.initSpec
    peer.initHeadIndex = this.initHeadIndex
    peer.isCorePhrase = this.isCorePhrase
    peer.coreHead = this.coreHead
    peer.units ++= this.units
    peer.headMap ++= this.headMap
    peer.bagOfHwds = this.bagOfHwds
    peer.extraPoint = this.extraPoint
    peer.candidateUpgraders ++= this.candidateUpgraders
    peer
  }
}

class UpgraderPhrase(_rule: Rule) extends Phrase(_rule) {
  val typo = Phrase.UPGRADER
  
  override def init(unit: EntryUnit, _initHead: Head, _bagOfHwds: HashSet[String]): Boolean = {
    initHead = _initHead.shallowCopy(this)
    bagOfHwds = _bagOfHwds
    var i = 0
    var succeeds = true
    if(rule.body.size > 1) {
      while(i < rule.body.size && succeeds) {
        val spec = rule.body.apply(i)
        if(!spec.forms.isEmpty) {
          succeeds = false
          var j = 0
          while(j < spec.forms.size && !succeeds) {
            succeeds = bagOfHwds.contains(spec.forms.apply(j))
            j += 1
          }
          if(spec.forms.contains(initHead.form))
            succeeds = succeeds && (if(spec.typo != Head.UNDEF) spec.typo == initHead.typo else true)
        }
        i += 1
      }
    }
    else {
      val spec = rule.body.head
      succeeds = (if(spec.typo != Head.UNDEF) spec.typo == initHead.typo else true)
    }
    if(succeeds) {
      if(unit.gloss == null && unit.isInstanceOf[Exa]) {
        units += unit.asInstanceOf[Exa].parent
      }
      else
        units += unit
      initHead.phrase = this
      initHeadIndex = getInitHeadIndex(initHead)
      rule.from = initHeadIndex
      rule.to = initHeadIndex
      initSpec = rule.body.apply(initHeadIndex)
//      initSpec = rule.fit(initHead, Rule.FORWARD)
      initSpec.typo = initHead.typo
      if(initSpec.id != null)
        headMap.put(initSpec.id, initHead)
//      rule.updateStatus
      rule.fittedVector.update(initHeadIndex, true)
      if(initSpec.isCore) {
        coreHead = initHead
        isCorePhrase = true
        coreHead.quality += rule.numberOfMissingSpecs * Rule.MIS_PT
      }
    }
    succeeds
  }
  
  def shallowCopy: UpgraderPhrase = {
    val peer = new UpgraderPhrase(rule.copy)
    peer.id = this.id
    peer.initHead = this.initHead
    peer.initSpec = this.initSpec
    peer.initHeadIndex = this.initHeadIndex
    peer.isCorePhrase = this.isCorePhrase
    peer.coreHead = this.coreHead
    peer.units ++= this.units
    peer.headMap ++= this.headMap
    peer.bagOfHwds = this.bagOfHwds
    peer
  }
  
  def upgrade(target: Phrase): Phrase = {
    var upgraded: Phrase = null
//    println(s"upgrade: isUpgradable(target)=${isUpgradable(target)}")
    if(isUpgradable(target)) {
      upgraded = target.deepCopy
      rule.category match {
        case Rule.SEL => upgradeBySelection(upgraded)
        case Rule.ALT => upgradeByAlteration(upgraded)
        case _ =>
      }
    }
    upgraded
  }
  
  def upgradeWholePhrase(target: Phrase, wholePhrase: Phrase): Phrase = {
//    println(s"upgradeWholePhrase: isUpgradable(target)=${isUpgradable(target)}")
    if(isUpgradable(target)) {
      val wholePhrasePeer = wholePhrase.deepCopy
      val targetPeer = wholePhrase.peerPhraseMap.apply(target.id)
      val originalQuality = targetPeer.coreHead.quality
//      println(s"this.units=${this.units}, targetPeer.units(before)=${targetPeer.units}")
      rule.category match {
        case Rule.SEL => upgradeBySelection(targetPeer)
        case Rule.ALT => upgradeByAlteration(targetPeer)
        case _ =>
      }
//      println(s"targetPeer.rule=${targetPeer.rule}, targetPeer.units(after)=${targetPeer.units}")
      wholePhrasePeer.coreHead.quality += targetPeer.coreHead.quality - originalQuality
      wholePhrasePeer
    }
    else
      null
  }
  
  def isUpgradable(target: Phrase): Boolean = {
    println(s"isUpgradable: condition=${target.coreHead != null && target.isDone && this.coreHead.fitsWith(target.coreHead)}, target.rule=${target.rule}, this.rule=${this.rule}")
    if(target.coreHead != null && target.isDone && this.coreHead.fitsWith(target.coreHead)) {
      this.rule.clearHeadIdVector
      var is = true
      var i = 0
      var direction = Rule.BACKWARD
      var numberOfFitted = 0
      while(i < rule.body.size && is == true) {
        val spec = rule.body.apply(i)
//        println(s"spec=${spec}, spec.level=${spec.level}")
        if(direction == Rule.BACKWARD && spec.isCore) {
          direction = Rule.FORWARD
        }
        if(!spec.isCore) {
          val child = target.coreHead.getChildInLevel(spec, spec.level, direction)
          if(child != null) {
            numberOfFitted += 1
            rule.headIdVector.update(i, child.id)
          }
          
//          is = target.coreHead.childExistsInLevel(spec, spec.level, direction)
//          if(is)
//            numberOfFitted += 1
        }
        else {
          rule.headIdVector.update(i, target.coreHead.id)
        }
        i += 1
      }
      println(s"isUpgradable: numberOfFitted=${numberOfFitted}")
      numberOfFitted == rule.body.size - 1
    }
    else
      false
  }
  
  private def upgradeBySelection(qualityTarget: Phrase) {
    qualityTarget.coreHead.quality += Rule.getPoint(this.rule)
    val unitsTarget = {
      if(isCorePhrase)
        qualityTarget
      else {
        val initHeadIndex = getInitHeadIndex(initHead)
        val coreIndex = rule.getCoreIndex
        val spec = rule.body.apply(initHeadIndex)
        if(initHeadIndex < coreIndex)
          qualityTarget.coreHead.getChildInLevel(spec, spec.level, Rule.BACKWARD).phrase
        else
          qualityTarget.coreHead.getChildInLevel(spec, spec.level, Rule.FORWARD).phrase
      }
    }
    unitsTarget.rule = this.rule
    unitsTarget.rule.finish
    val toBeRemoved = new MutableList[EntryUnit]
    unitsTarget.units.foreach(unit => {
      if(!this.units.contains(unit)) {
        toBeRemoved += unit
      }
    })
    toBeRemoved.foreach(unitsTarget.units.remove(_))
  }
  
  private def upgradeByAlteration(target: Phrase) {
    target.coreHead.quality += Rule.getPoint(this.rule)
    target.rule = this.rule
    target.rule.finish
    target.units.clear
    target.units ++= this.units
  }
  
  def absolve(anotherPhrase: Phrase, direction: String): Phrase = {
    var resultPhrase: Phrase = null
    val spec = rule.fit(anotherPhrase.coreHead, direction)
    if(spec != null) {
      if(spec.isCore) {
        this.coreHead = anotherPhrase.coreHead
      }
      resultPhrase = this.deepCopy
      resultPhrase.rule.updateStatus
    }
    resultPhrase
  }
}

class MWEPhrase(_rule: Rule) extends Phrase(_rule) {
  val typo = Phrase.MWE
  
  var entry: Entry = null
  
  var mweHwd: String = null
  
  def init(_mweHwd: String, _entry: Entry, unit: EntryUnit, _initHead: Head, _bagOfHwds: HashSet[String]): Boolean = {
    mweHwd = _mweHwd
    initHead = _initHead.shallowCopy(this)
    bagOfHwds = _bagOfHwds
//    println(s"initMWEPhrase: bagOfHwds: ${bagOfHwds}")
    var i = 0
    var succeeds = true
    while(i < rule.body.size && succeeds) {
      val spec = rule.body.apply(i)
      if(!spec.forms.isEmpty) {
        succeeds = false
        var j = 0
        while(j < spec.forms.size && !succeeds) {
          succeeds = bagOfHwds.contains(spec.forms.apply(j))
          j += 1
        }
      }
      i += 1
    }
    if(succeeds) {
      entry = _entry
      if(unit != null)
        units += unit
      initHead.phrase = this
      initHeadIndex = getInitHeadIndex(initHead)
      rule.from = initHeadIndex
      rule.to = initHeadIndex
      initSpec = rule.fit(initHead, Rule.FORWARD)
      if(initSpec.id != null)
        headMap.put(initSpec.id, initHead)
      if(initSpec.isCore) {
        coreHead = initHead
        coreHead.id = "c"
        isCorePhrase = true
      }
      rule.updateStatus
    }
    succeeds
  }
  
  def absolve(anotherPhrase: Phrase, direction: String): Phrase = {
    var resultPhrase: Phrase = null
    val incomingHead = anotherPhrase.coreHead
    if(incomingHead != null && incomingHead.isPure && (if(coreHead != null) coreHead.isPure else true)) {
      val spec = rule.fit(incomingHead, direction)
      if(spec != null) {
        val thisPeer = this.deepCopy.asInstanceOf[MWEPhrase]
        val anotherPhrasePeer = anotherPhrase.deepCopy
        resultPhrase = thisPeer.absolveMWEComponentHead(spec, anotherPhrasePeer, direction)
      }
    }
    resultPhrase
  }
  
  private def absolveMWEComponentHead(spec: Spec, anotherPhrase: Phrase, direction: String): Phrase = {
    if(spec.isCore) {
      coreHead = anotherPhrase.coreHead
    }
    rule.updateStatus
    if(rule.status == Rule.DONE) {
      var resultPhrase = rule.createProductPhrase(this)
//      println(s"absolveMWEComponentHead: resultPhrase=${resultPhrase}, coreHead=${coreHead}")
      if(resultPhrase != null) {
        // perform operations on the product
        if(resultPhrase.rule.isComplete && !resultPhrase.rule.product.operations.isEmpty) {
          resultPhrase.coreHead.performOperations(resultPhrase.rule.product.operations, headMap)
        }
      }
      resultPhrase
    }
    else
      this
  }
  
  def transferMWE: MutableList[Phrase] = {
    val list = new MutableList[Phrase]
    rule.product.typo match {
      case POS.NOUN => {
        bagOfHwds += mweHwd
//        println(s"transferMWE, as noun: mweHwd=${mweHwd}, entry=${entry.name}, bagOfHwds=${bagOfHwds}")
        val morph = new NounMorph(mweHwd, entry, coreHead.get(AspectName.PERSON_NUMBER), coreHead.originality)
        if(units.isEmpty) {
          list ++= morph.createPhrases(bagOfHwds)
        }
        else {
          list ++= morph.createPhrasesForUnits(units, bagOfHwds)
        }
        list.foreach(phrase => {
          rule.product.stringAspects.foreach(stringAspect => {
            phrase.initHead.set(stringAspect._1, stringAspect._2)
          })
          phrase.initHead.quality += Rule.MWE_PT * (rule.body.size - 1)
          println(s"transferMWE: ${phrase}")
        })
      }
      case POS.VERB => {
        bagOfHwds += mweHwd
//        println(s"transferMWE, as verb: hwd=${mweHwd}, coreHead=${coreHead}")
        val morph = new VerbMorph(mweHwd, entry, POS.VERB, coreHead.get(AspectName.IS_STEM), 
              coreHead.get(AspectName.FUNCTION), coreHead.get(AspectName.TENSE), coreHead.get(AspectName.PERSON_NUMBER), coreHead.originality)
        if(units.isEmpty) {
          list ++= morph.createPhrases(bagOfHwds)
        }
        else {
          list ++= morph.createPhrasesForUnits(units, bagOfHwds)
        }
        list.foreach(phrase => {
          rule.product.stringAspects.foreach(stringAspect => {
            phrase.initHead.set(stringAspect._1, stringAspect._2)
          })
          phrase.initHead.quality += Rule.MWE_PT * (rule.body.size - 1)
          println(s"transferMWE: ${phrase}")
        })
      }
      case POS.ADJECTIVE => {
        bagOfHwds += mweHwd
//        println(s"transferMWE, as adjective: mweHwd=${mweHwd}, entry=${entry.name}")
        var degree = coreHead.get(AspectName.DEGREE)
        if(degree == null)
          degree = AspectValue.POSI
        val morph = new AdjectiveMorph(mweHwd, entry, degree, coreHead.originality)
        if(units.isEmpty) {
          list ++= morph.createPhrases(bagOfHwds)
        }
        else {
          list ++= morph.createPhrasesForUnits(units, bagOfHwds)
        }
        list.foreach(phrase => {
          rule.product.stringAspects.foreach(stringAspect => {
            phrase.initHead.set(stringAspect._1, stringAspect._2)
          })
          phrase.initHead.quality += Rule.MWE_PT * (rule.body.size - 1)
          println(s"transferMWE: ${phrase}")
        })
      }
      case _ =>
    }
    list
  }
  
  def shallowCopy: Phrase = {
    val peer = new MWEPhrase(rule.copy)
    peer.id = this.id
    peer.initHead = this.initHead
    peer.initSpec = this.initSpec
    peer.initHeadIndex = this.initHeadIndex
    peer.isCorePhrase = this.isCorePhrase
    peer.coreHead = this.coreHead
    peer.units ++= this.units
    peer.entry = this.entry
    peer.headMap ++= this.headMap
    peer.bagOfHwds = this.bagOfHwds
    peer.mweHwd = this.mweHwd
    peer.extraPoint = this.extraPoint
    peer
  }
}
