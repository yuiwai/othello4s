package othello.core

final case class Game(
  othello: Othello,
  state: GameState,
  ownerId: ParticipantId,
  challengerId: Option[ParticipantId],
  nextIsOwner: Boolean,
  version: Int) {
  def putStone(actorId: ParticipantId, pos: Pos): Either[GameError, Game] =
    (state, isTurnOf(actorId)) match {
      case (Playing, true) =>
        othello.put(pos, othello.turn)
          .fold(e => Left(InvalidPos(e)), o => Right(copy(o, nextIsOwner = !nextIsOwner, version = version + 1)))
      case (Playing, false) => Left(IsNotTurn)
      case _ => Left(NotPlaying)
    }
  def isTurnOf(participantId: ParticipantId): Boolean =
    (isOwner(participantId) && nextIsOwner) || (isChallenger(participantId) && !nextIsOwner)
  def isOwner(participantId: ParticipantId): Boolean = participantId == ownerId
  def isChallenger(participantId: ParticipantId): Boolean = challengerId.contains(participantId)
  def entry(newChallengerId: ParticipantId): Either[GameError, Game] = // FIXME state == Waitingも見た方がいい
    challengerId.fold[Either[GameError, Game]] {
      if (ownerId == newChallengerId) Left(SameAsOwnerId)
      else Right(copy(challengerId = Some(newChallengerId), state = Prepared))
    }(_ => Left(NotWaiting))
  def start: Game = copy(state = Playing)
  def giveUp: Game = copy(state = Terminated)
}
object Game {
  def apply(ownerId: ParticipantId): Game = apply(Othello(), Waiting, ownerId, None, nextIsOwner = true, 1)
}

// FIXME Stateごとに持つ状態が違うので、それらはStateに持たせるというのも一つの案
sealed trait GameState
case object Waiting extends GameState
case object Prepared extends GameState
case object Canceled extends GameState
case object Playing extends GameState
case object Terminated extends GameState

trait GameOps
trait Player

sealed trait GameError
case object NotWaiting extends GameError
case object SameAsOwnerId extends GameError
case object IsNotTurn extends GameError
case object NotPlaying extends GameError
case object NotPrepared extends GameError
case object AlreadyTerminated extends GameError
final case class InvalidPos(cause: OthelloError) extends GameError
