package dev.rudiments.hardcore

sealed trait Message extends Product
sealed trait In extends Message
sealed trait Out extends Message

trait Command extends In
trait Query extends In
trait Event extends Out
trait Report extends Out
trait Error extends Out