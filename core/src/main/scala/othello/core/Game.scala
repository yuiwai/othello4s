package othello.core

final case class Game(
  othello: Othello,
  state: GameState,
  ownerId: ParticipantId,
  challengerId: Option[ParticipantId],
  nextIsOwner: Boolean,
  version: GameVersion) {
  def putStone(actorId: ParticipantId, pos: Pos): Either[GameError, Game] =
    (state, isTurnOf(actorId)) match {
      case (Playing, true) =>
        othello.put(pos, othello.turn)
          .fold(
            e => Left(InvalidPos(e)),
            o => Right(copy(o, version = version.increment, nextIsOwner = !nextIsOwner))
              .map { g =>
                g.copy(
                  state = if (o.isGameOver) Terminated(g.greaterColor.flatMap(g.participantIdFromColor))
                  else Playing)
              })
      case (Playing, false) => Left(IsNotTurn)
      case _ => Left(NotPlaying)
    }
  def pass(actorId: ParticipantId): Either[GameError, Game] =
    (state, isTurnOf(actorId)) match {
      case (Playing, true) =>
        othello.pass match {
          case Right(o) => Right(copy(othello = o, version = version.increment, nextIsOwner = !nextIsOwner))
          case _ => Right(this) // TODO エラーをちゃんと伝搬する
        }
      case (Playing, false) => Left(IsNotTurn)
      case _ => Left(NotPlaying)
    }
  def isTurnOf(participantId: ParticipantId): Boolean =
    (isOwner(participantId) && nextIsOwner) || (isChallenger(participantId) && !nextIsOwner)
  def isOwner(participantId: ParticipantId): Boolean = participantId == ownerId
  def isChallenger(participantId: ParticipantId): Boolean = challengerId.contains(participantId)
  def isPlaying: Boolean = state.isInstanceOf[Playing.type]
  def isTerminated: Boolean = state.isInstanceOf[Terminated]
  def mode(participantId: ParticipantId): GameMode =
    if (Set(isOwner _, isChallenger _).exists(_ (participantId))) PlayerMode else WatchingMode
  def canAcceptVersion(targetVersion: GameVersion): Boolean = isPlaying && version.accept(targetVersion).isDefined
  def currentTurn: Option[ParticipantId] = participantIdFromColor(othello.turn)
  def entry(newChallengerId: ParticipantId): Either[GameError, Game] = // FIXME state == Waitingも見た方がいい
    challengerId.fold[Either[GameError, Game]] {
      if (ownerId == newChallengerId) Left(SameAsOwnerId)
      else Right(copy(challengerId = Some(newChallengerId), state = Prepared))
    }(_ => Left(NotWaiting))
  def participantIdFromColor(color: StoneColor): Option[ParticipantId] =
    (othello.turn == color, nextIsOwner) match {
      case (true, true) | (false, false) => Some(ownerId)
      case _ => challengerId
    }
  def greaterColor: Option[StoneColor] = othello.greaterColor
  def winner: Option[ParticipantId] = state match {
    case Terminated(p) => p
    case _ => None
  }
  def start: Game = copy(state = Playing)
  // TODO version更新など、共通系の処理が漏れるので、状態更新処理フローを共通化したい
  def giveUp: Game = copy(state = Terminated(if (nextIsOwner) challengerId else Some(ownerId)), version = version.increment)
  def timeout: Game = copy(state = Terminated(if (nextIsOwner) challengerId else Some(ownerId)), version = version.increment)
}
object Game {
  def apply(ownerId: ParticipantId): Game = apply(Othello(), Waiting, ownerId, None, nextIsOwner = true, GameVersion.first)
  def apply(ownerId: ParticipantId, board: Board): Game =
    apply(Othello(board, Black), Waiting, ownerId, None, nextIsOwner = true, GameVersion.first)
  def singlePlay: Game = apply(Othello(), Playing, ParticipantId(-1), Some(ParticipantId(-2)), nextIsOwner = true, GameVersion.first)
}

final case class GameVersion(value: Int) extends AnyVal {
  def increment: GameVersion = GameVersion(value + 1)
  def accept(version: GameVersion): Option[GameVersion] = Some(version).filter(_ == increment)
}
object GameVersion {
  def first: GameVersion = apply(1)
}

sealed trait GameMode
case object PlayerMode extends GameMode
case object WatchingMode extends GameMode

sealed trait GameState
case object Waiting extends GameState
case object Prepared extends GameState
case object Canceled extends GameState
case object Playing extends GameState
case class Terminated(participantId: Option[ParticipantId]) extends GameState

sealed trait GameError
case object NotWaiting extends GameError
case object SameAsOwnerId extends GameError
case object IsNotTurn extends GameError
case object NotPlaying extends GameError
case object NotPrepared extends GameError
case object AlreadyTerminated extends GameError
final case class InvalidPos(cause: OthelloError) extends GameError
