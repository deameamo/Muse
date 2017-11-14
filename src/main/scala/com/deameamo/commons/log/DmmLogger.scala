package com.deameamo.commons.log

import org.apache.log4j.{LogManager, Logger}

/**
  * Created by Huangyu on 14/11/2017.
  */
object DmmLogger {

  private val DEFAULT_LOGGER_NAME = "DmmLogger"

  val logger: Logger = Logger.getLogger(DEFAULT_LOGGER_NAME)
}

