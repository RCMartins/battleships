package pt.rmartins.battleships.frontend.views.game

import io.udash.ReadableProperty

object Utils {

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

  def combine[A, B, C, D, E](
      propertyA: ReadableProperty[A],
      propertyB: ReadableProperty[B],
      propertyC: ReadableProperty[C],
      propertyD: ReadableProperty[D],
      propertyE: ReadableProperty[E]
  ): ReadableProperty[(A, B, C, D, E)] =
    combine(propertyA, propertyB, propertyC, propertyD)
      .combine(propertyE) { case ((a, b, c, d), e) => (a, b, c, d, e) }

  def combine[A, B, C, D, E, F](
      propertyA: ReadableProperty[A],
      propertyB: ReadableProperty[B],
      propertyC: ReadableProperty[C],
      propertyD: ReadableProperty[D],
      propertyE: ReadableProperty[E],
      propertyF: ReadableProperty[F]
  ): ReadableProperty[(A, B, C, D, E, F)] =
    combine(propertyA, propertyB, propertyC, propertyD, propertyE)
      .combine(propertyF) { case ((a, b, c, d, e), f) => (a, b, c, d, e, f) }

  def addSeparator[A](list: List[A], separator: A): List[A] = {
    list match {
      case value1 :: value2 :: next =>
        value1 :: separator :: addSeparator(value2 :: next, separator)
      case list =>
        list
    }
  }

}
