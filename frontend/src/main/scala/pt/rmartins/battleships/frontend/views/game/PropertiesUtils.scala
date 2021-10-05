package pt.rmartins.battleships.frontend.views.game

import io.udash.ReadableProperty

object PropertiesUtils {

  def combine[A, B](
      propertyA: ReadableProperty[A],
      propertyB: ReadableProperty[B]
  ): ReadableProperty[(A, B)] =
    propertyA.combine(propertyB) { case (a, b) => (a, b) }

  def combine[A, B, C](
      propertyA: ReadableProperty[A],
      propertyB: ReadableProperty[B],
      propertyC: ReadableProperty[C]
  ): ReadableProperty[(A, B, C)] =
    combine(propertyA, propertyB)
      .combine(propertyC) { case ((a, b), c) => (a, b, c) }

  def combine[A, B, C, D](
      propertyA: ReadableProperty[A],
      propertyB: ReadableProperty[B],
      propertyC: ReadableProperty[C],
      propertyD: ReadableProperty[D]
  ): ReadableProperty[(A, B, C, D)] =
    combine(propertyA, propertyB, propertyC)
      .combine(propertyD) { case ((a, b, c), d) => (a, b, c, d) }

}
