package work.unformed.hardcore.dsl

trait Tx

trait ReadOnly extends Tx
trait Appendable extends Tx
trait Exclusive extends Tx