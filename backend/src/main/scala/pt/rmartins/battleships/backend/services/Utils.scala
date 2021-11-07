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

  def combinations[A](ls: List[A], amount: Int): List[List[A]] =
    if (amount == 0)
      List(Nil)
    else
      ls match {
        case Nil =>
          Nil
        case head :: tail =>
          combinations(tail, amount) ++ combinations(tail, amount - 1).map(head :: _)
      }

}
