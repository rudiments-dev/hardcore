package dev.rudiments.gates.h2

import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore.{Location, Tx}
import scalikejdbc.{DB, DBSession}

class H2Gate(config: H2Config) extends Location[Schema, Schema]{

}

class H2Tx extends Tx with StrictLogging { //TODO make session an object inside story?
  val session: DBSession = DB.readOnlySession()
  logger.info("AutoTx initiated: {}", session)
}