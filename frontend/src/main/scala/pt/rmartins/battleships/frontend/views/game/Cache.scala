package pt.rmartins.battleships.frontend.views.game

import scala.collection.mutable

class Cache[K, V] {

  private val cache: mutable.Map[K, V] = mutable.Map.empty

  def contains(key: K): Boolean =
    cache.contains(key)

  def get(key: K): Option[V] =
    cache.get(key)

  def getOrUpdate(key: K, value: => V): V =
    cache.getOrElseUpdate(key, value)

}
