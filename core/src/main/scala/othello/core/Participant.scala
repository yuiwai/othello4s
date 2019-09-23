package othello.core

sealed trait Participant {
  val name: ParticipantName
}
final case class Free() extends Participant
final case class Wait() extends Participant
final case class Watching() extends Participant
final case class Gaming() extends Participant

final case class ParticipantId(value: Int) extends AnyVal
final case class ParticipantName(value: String) extends AnyVal
