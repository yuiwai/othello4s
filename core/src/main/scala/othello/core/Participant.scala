package othello.core

final case class Participant(
  name: ParticipantName,
  state: ParticipantState)
final case class ParticipantId(value: Int) extends AnyVal
final case class ParticipantName(value: String) extends AnyVal
object Participant {
  def apply(name: ParticipantName): Participant = apply(name, Free)
}
object ParticipantName {
  val you = apply("あなた")
  val ai: ParticipantName = apply("AI")
  val noName = apply("No Name")
}

sealed trait ParticipantState
case object Free extends ParticipantState
case object Wait extends ParticipantState
case object Watching extends ParticipantState
case object Gaming extends ParticipantState
