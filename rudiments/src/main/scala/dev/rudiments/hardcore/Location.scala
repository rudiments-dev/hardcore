package dev.rudiments.hardcore

trait Location[K, +V] {}
class Place extends Store[Location[_, _], Location[_, _]] {}
class Satellite[K, +V] extends Location[K, V] {}
class Index[V] extends Location[V, Set[ID[_]]] {}
class Matrix[C, R, +V] extends Location[(C, R), V] {}
class Gate extends Place {}
