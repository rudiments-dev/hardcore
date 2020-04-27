package dev.rudiments.hardcore

package object types {
  trait DTO extends Product

  trait Ref
  trait Instance extends Ref
  trait ID extends Ref
  trait AutoID extends ID
}
