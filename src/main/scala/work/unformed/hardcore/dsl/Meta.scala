package work.unformed.hardcore.dsl

class Meta[A] {}

sealed trait Ref[A]
class ID[A] extends Ref[A] {}
class Instance[A] extends Ref[A] {}