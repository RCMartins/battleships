package pt.rmartins.battleships.backend.services

object Utils {

  def combine[A, B](
      optionA: Option[A],
      optionB: Option[B]
  ): Option[(A, B)] =
    (optionA, optionB) match {
      case (Some(valueA), Some(valueB)) => Some((valueA, valueB))
      case _                            => None
    }

  def combine[A, B, C](
      optionA: Option[A],
      optionB: Option[B],
      optionC: Option[C]
  ): Option[(A, B, C)] =
    (optionA, optionB, optionC) match {
      case (Some(valueA), Some(valueB), Some(valueC)) => Some((valueA, valueB, valueC))
      case _                                          => None
    }

}
