package dev.rudiments.gates.h2

import com.typesafe.scalalogging.StrictLogging
import org.h2.tools.Server

object H2Server extends scala.App with StrictLogging {
  Server
    .createTcpServer("-tcp", "-tcpAllowOthers", "-tcpPort", "9092", "-ifNotExists")
    .start()
  logger.info("H2 server started at localhost:9092")
}
