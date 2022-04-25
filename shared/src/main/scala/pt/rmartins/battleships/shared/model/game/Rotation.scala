package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.misc.{EnumCtx, ValueEnum, ValueEnumCompanion}

sealed case class Rotation private (rIndex: Int)(implicit val enumCtx: EnumCtx) extends ValueEnum {

  def next: Rotation = rotateBy(1)

  def rotateBy(delta: Int): Rotation = Rotation.all(((rIndex + delta) % 4 + 4) % 4)

}

object Rotation extends ValueEnumCompanion[Rotation] {

  final val Rotation0: Value = new Rotation(0)
  final val Rotation1: Value = new Rotation(1)
  final val Rotation2: Value = new Rotation(2)
  final val Rotation3: Value = new Rotation(3)

  val all = Seq(Rotation0, Rotation1, Rotation2, Rotation3)

  private val allMap: Map[Int, Rotation] =
    all.map(rotation => rotation.rIndex -> rotation).toMap

  def getUnsafe(rIndex: Int): Rotation =
    allMap(rIndex)

}
