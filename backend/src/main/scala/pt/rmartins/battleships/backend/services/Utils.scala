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

  def combinationGroups[A](
      list: List[A],
      amountGroups: List[Int]
  ): Set[Set[Set[A]]] = {
    def loop(
        list: List[A],
        other: List[A],
        amountGroups: List[Int]
    ): List[List[List[A]]] =
      amountGroups match {
        case Nil =>
          List(Nil)
        case 0 :: nextAmounts =>
          loop(list ++ other, Nil, nextAmounts).map(Nil :: _)
        case amount :: nextAmounts =>
          list match {
            case Nil =>
              Nil
            case head :: tail =>
              loop(tail, head :: other, amountGroups) ++
                loop(tail, other, (amount - 1) :: nextAmounts)
                  .map {
                    case Nil                    => Nil
                    case headGroup :: nextGroup => (head :: headGroup) :: nextGroup
                  }
          }
      }

    loop(list, Nil, amountGroups).map(_.map(_.toSet).toSet).toSet
  }

}
