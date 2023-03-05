package dev.rudiments.logs

import org.slf4j.{Logger, LoggerFactory}

trait Log {
  lazy val log: Logger = LoggerFactory.getLogger(this.getClass)
}
