package othello.service

import othello.core._

import scala.language.higherKinds

trait Service[F[_]] {
  def participate: F[ParticipantId]
  def allGames(participantId: ParticipantId): F[Seq[GameSummary]]
  def game(gameId: GameId): F[Option[Game]]
  def createGame(participantId: ParticipantId): F[Either[ServiceError, GameSummary]]
  def entry(gameId: GameId, participantId: ParticipantId): F[Either[ServiceError, EntryId]]
  def putStone(gameId: GameId, participantId: ParticipantId, pos: Pos): F[Either[ServiceError, Game]]
  def giveUp(gameId: GameId, participantId: ParticipantId): F[Either[ServiceError, Game]]
}

final case class GameId(value: Int) extends AnyVal
trait GameRepository[F[_]] {
  def all: F[Seq[(GameId, Game)]]
  def find(gameId: GameId): F[Option[Game]]
  def create(ownerId: ParticipantId): F[GameId]
  def ownedBy(ownerId: ParticipantId): F[Option[Game]]
  def store(gameId: GameId, game: Game): F[Option[Game]] // FIXME 戻り値要検討
  def delete(gameId: GameId): F[Option[GameId]]
}

final case class GameSummary(
  gameId: GameId,
  gameState: GameState,
  ownerId: ParticipantId,
  challengerId: Option[ParticipantId]
) {
  def isPlaying: Boolean = gameState == Playing
  def isParticipating(participantId: ParticipantId): Boolean =
    ownerId == participantId || challengerId.contains(participantId)
}

sealed trait ServiceError
case object DecodeError extends ServiceError
case object ParticipantNotFound extends ServiceError
case object GameNotFound extends ServiceError
case object OwnedGameExists extends ServiceError
case object GameStarted extends ServiceError
case object PutError extends ServiceError
case object GameStoreFailed extends ServiceError

object ServiceError {
  def all: Seq[ServiceError] = Seq(
    DecodeError,
    ParticipantNotFound,
    GameNotFound,
    OwnedGameExists
  )
  // TODO impl
  def apply(e: String): Option[ServiceError] = None
}

trait ParticipantRepository[F[_]] {
  def find(participantId: ParticipantId): F[Option[Participant]]
  def create: F[ParticipantId]
}

final case class EntryId(value: Int) extends AnyVal
trait Entry {
  val gameId: GameId
  val entryId: EntryId
}

final case class EntryRequest(gameId: GameId, participantId: ParticipantId)
final case class PutStoneRequest(gameId: GameId, participantId: ParticipantId, pos: Pos)
final case class GiveUpRequest(gameId: GameId, participantId: ParticipantId)

sealed trait GameEvent {
  val gameId: GameId
}
final case class StonePut(gameId: GameId, participantId: ParticipantId, pos: Pos, version: GameVersion) extends GameEvent
final case class GivenUp(gameId: GameId, version: GameVersion) extends GameEvent
final case class Terminated(gameId: GameId, version: GameVersion) extends GameEvent
