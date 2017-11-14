package com.deameamo.commons.log

import java.text.SimpleDateFormat
import java.util._

import org.apache.log4j.helpers.LogLog
import org.apache.log4j.spi.LoggingEvent
import org.apache.log4j.{FileAppender, Priority}

/**
  * Created by Huangyu on 01/09/2017.
  */
object SeparateFileAppender {

  val TOP_OF_TROUBLE: Int = -1
  val TOP_OF_MINUTE: Int = 0
  val TOP_OF_HOUR: Int = 1
  val HALF_DAY: Int = 2
  val TOP_OF_DAY: Int = 3
  val TOP_OF_WEEK: Int = 4
  val TOP_OF_MONTH: Int = 5

  val STRATEGY_ABOVE = "above"
  val STRATEGY_EQUAL = "equal"

  def main(args: Array[String]): Unit = {

    println("done")
  }
}

class SeparateFileAppender() extends FileAppender() {

  private var datePattern = "'.'yyyy-MM-dd"
  private var strategy = SeparateFileAppender.STRATEGY_ABOVE

  def setDatePattern(pattern: String): Unit = {
    datePattern = pattern
  }

  def getDatePattern: String = datePattern

  def setStrategy(strategy: String): Unit = {
    strategy match {
      case SeparateFileAppender.STRATEGY_ABOVE => this.strategy = SeparateFileAppender.STRATEGY_ABOVE
      case SeparateFileAppender.STRATEGY_EQUAL => this.strategy = SeparateFileAppender.STRATEGY_EQUAL
      case _ => this.strategy = SeparateFileAppender.STRATEGY_ABOVE
    }
  }

  def getStrategy: String = strategy

  override def isAsSevereAsThreshold(priority: Priority): Boolean = {
    strategy match {
      case SeparateFileAppender.STRATEGY_ABOVE =>
        threshold == null || priority.isGreaterOrEqual(threshold)
      case SeparateFileAppender.STRATEGY_EQUAL =>
        threshold == null || priority.equals(threshold)
      case _ =>
        true
    }
  }

  private var nextCheck = System.currentTimeMillis() - 1

  private val now = new Date()

  private var sdf: SimpleDateFormat = _

  private val rc: RollingCalendar = new RollingCalendar()

  private val GMT_TIME_ZONE = TimeZone.getTimeZone("GMT")

  private var baseFileName: String = _

  override def activateOptions(): Unit = {
    if (baseFileName == null) {
      baseFileName = fileName
    }
    if (datePattern != null && baseFileName != null) {
      now.setTime(System.currentTimeMillis())
      sdf = new SimpleDateFormat(datePattern)
      val typo = computeCheckPeriod
      printPeriodicity(typo)
      rc.typo = typo
      fileName = baseFileName + sdf.format(now)
      super.activateOptions()
    } else {
      LogLog.error("Either File or DatePattern options are not set for appender [$name].")
    }
  }

  private def printPeriodicity(typo: Int): Unit = {
    typo match {
      case SeparateFileAppender.TOP_OF_MINUTE =>
        LogLog.debug(s"Appender $name to be rolled every minute.")
      case SeparateFileAppender.TOP_OF_HOUR =>
        LogLog.debug(s"Appender $name to be rolled on top of every hour.")
      case SeparateFileAppender.HALF_DAY =>
        LogLog.debug(s"Appender $name to be rolled at midday and midnight.")
      case SeparateFileAppender.TOP_OF_DAY =>
        LogLog.debug(s"Appender $name to be rolled at midnight.")
      case SeparateFileAppender.TOP_OF_WEEK =>
        LogLog.debug(s"Appender $name to be rolled at the start of week.")
      case SeparateFileAppender.TOP_OF_MONTH =>
        LogLog.debug(s"Appender $name to be rolled at the start of every month.")
      case _ =>
        LogLog.warn(s"Unknown periodicity for appender $name.")
    }
  }

  private def computeCheckPeriod: Int = {
    val rollingCalendar = new RollingCalendar(GMT_TIME_ZONE, Locale.getDefault)
    val epoch = new Date(0)
    if (datePattern != null) {
      for (i <- SeparateFileAppender.TOP_OF_MINUTE until SeparateFileAppender.TOP_OF_MONTH) {
        val simpleDateFormat = new SimpleDateFormat(datePattern)
        simpleDateFormat.setTimeZone(GMT_TIME_ZONE)
        val r0 = simpleDateFormat.format(epoch)
        rollingCalendar.typo = i
        val next = new Date(rollingCalendar.getNextCheckMillis(epoch))
        val r1 = simpleDateFormat.format(next)
        if (r0 != r1) {
          return i
        }
      }
    }
    SeparateFileAppender.TOP_OF_TROUBLE
  }

  private def rollOver(): Unit = {
    if (datePattern == null) {
      errorHandler.error("Missing DatePattern option in rollOver().")
      return
    }

    val currentFileName = baseFileName + sdf.format(now)

    if (currentFileName == fileName) {
      return
    }

    this.closeFile()

    fileName = currentFileName
    super.activateOptions()
  }

  override protected def subAppend(event: LoggingEvent): Unit = {
    val n = System.currentTimeMillis()
    if (n > nextCheck) {
      now.setTime(n)
      nextCheck = rc.getNextCheckMillis(now)
      try {
        rollOver()
      } catch {
        case ex: Exception =>
          if (ex.isInstanceOf[InterruptedException]) {
            Thread.currentThread().interrupt()
          }
          LogLog.error("rollOver() failed.", ex)
      }
    }
    super.subAppend(event)
  }
}

class RollingCalendar(timeZone: TimeZone, locale: Locale) extends GregorianCalendar(timeZone, locale) {

  def this() = this(TimeZone.getDefault, Locale.getDefault(Locale.Category.FORMAT))

  var typo: Int = SeparateFileAppender.TOP_OF_TROUBLE

  def getNextCheckMillis(now: Date): Long = getNextCheckDate(now).getTime

  def getNextCheckDate(now: Date): Date = {
    setTime(now)

    typo match {
      case SeparateFileAppender.TOP_OF_MINUTE =>
        set(Calendar.MILLISECOND, 0)
        set(Calendar.SECOND, 0)
        add(Calendar.MINUTE, 1)
      case SeparateFileAppender.TOP_OF_HOUR =>
        set(Calendar.MILLISECOND, 0)
        set(Calendar.SECOND, 0)
        set(Calendar.MINUTE, 0)
        add(Calendar.HOUR_OF_DAY, 1)
      case SeparateFileAppender.HALF_DAY =>
        set(Calendar.MILLISECOND, 0)
        set(Calendar.SECOND, 0)
        set(Calendar.MINUTE, 0)
        val hour = get(Calendar.HOUR_OF_DAY)
        if (hour < 12) {
          set(Calendar.HOUR_OF_DAY, 12)
        } else {
          set(Calendar.HOUR_OF_DAY, 0)
          add(Calendar.DAY_OF_MONTH, 1)
        }
      case SeparateFileAppender.TOP_OF_DAY =>
        set(Calendar.MILLISECOND, 0)
        set(Calendar.SECOND, 0)
        set(Calendar.MINUTE, 0)
        set(Calendar.HOUR_OF_DAY, 0)
        add(Calendar.DATE, 1)
      case SeparateFileAppender.TOP_OF_WEEK =>
        set(Calendar.MILLISECOND, 0)
        set(Calendar.SECOND, 0)
        set(Calendar.MINUTE, 0)
        set(Calendar.HOUR_OF_DAY, 0)
        set(Calendar.DAY_OF_WEEK, getFirstDayOfWeek)
        add(Calendar.WEEK_OF_YEAR, 1)
      case SeparateFileAppender.TOP_OF_MONTH =>
        set(Calendar.MILLISECOND, 0)
        set(Calendar.SECOND, 0)
        set(Calendar.MINUTE, 0)
        set(Calendar.HOUR_OF_DAY, 0)
        set(Calendar.DATE, 1)
        add(Calendar.MONTH, 1)
      case _ => throw new IllegalStateException("Unknown periodicity type.")
    }

    getTime
  }
}

