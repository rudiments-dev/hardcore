package dev.rudiments.utils

class SaltedMap[K, +V](values: Array[(K, V)]) extends Map[K, V]{
  val salt: Int = 0;//TODO

  override def removed(key: K): SaltedMap[K, V] = new SaltedMap(values.filterNot(_._2 == key))

  override def updated[V1 >: V](key: K, value: V1): SaltedMap[K, V1] = ???/*this.get(key) match {
    case Some(v) if v == value => this
    case Some(_) =>
      new SaltedMap[K, V1](
        values.update(saltedHash(key), key -> value.asInstanceOf[V]).asInstanceOf[Array[(K, V1)]]
      ) //TODO improve, prob combine with get
    case None => new SaltedMap(values :+ key -> value)
  }*/

  override def get(key: K): Option[V] = {
    val found = values(saltedHash(key))
    if(found._1 == key) Some(found._2) else None
  }

  override def iterator: Iterator[(K, V)] = values.iterator

  def saltedHash(key: K): Int = (key.hashCode() + salt) % values.length
}

object SaltedMap extends Log {
  def empty[K, V]: SaltedMap[K, V] = new SaltedMap[K, V](Array.empty)

  def apply[K, V](pairs: (K, V)*): SaltedMap[K, V] = new SaltedMap[K, V](pairs.toArray)


  def h[K](key: K, size: Int, salt: Int): Int = {
    val hash = 31 * 7 + salt
    (31 * hash + key.hashCode()) % size
  }

  val maxInterations: Int = Int.MaxValue
  def salty[K](keys: List[K]): Int = {
    var i: Int = 1
    var fit: Boolean = false
    val expected = keys.indices
    while(i <= maxInterations && !fit) {
      fit = keys.map(k => h(k, keys.size, i)) == expected
      if(!fit && i % 10_000_000 == 0) {
        log.info("Failed on {}", i)
      }
      i += 1
    }
    if(!fit) {
      -1
    } else {
      i
    }
  }
}