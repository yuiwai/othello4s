package othello.service

import othello.core._

import scala.language.higherKinds

trait Service[F[_]] {
  def participate(name: ParticipantName): F[ParticipantId]
  def allGames(participantId: ParticipantId): F[Seq[GameSummary]]
  def game(gameId: GameId): F[Option[GameDetail]]
  def createGame(participantId: ParticipantId): F[Either[ServiceError, GameSummary]]
  def cancel(gameId: GameId, participantId: ParticipantId): F[Option[ServiceError]]
  def entry(gameId: GameId, participantId: ParticipantId): F[Either[ServiceError, Game]]
  def start(gameId: GameId, ownerId: ParticipantId): F[Either[ServiceError, Game]]
  def putStone(gameId: GameId, participantId: ParticipantId, pos: Pos): F[Either[ServiceError, Game]]
  def pass(gameId: GameId, participantId: ParticipantId): F[Either[ServiceError, Game]]
  def giveUp(gameId: GameId, participantId: ParticipantId): F[Either[ServiceError, Game]]

  // DEBUG
  def createCustomGame(participantId: ParticipantId, board: Board): F[Option[GameId]]
}

final case class GameId(value: Int) extends AnyVal
trait GameRepository[F[_]] {
  def all: F[Seq[(GameId, Game)]]
  def find(gameId: GameId): F[Option[Game]]
  def ownedBy(ownerId: ParticipantId): F[Option[Game]]
  def store(gameId: GameId, game: Game): F[Option[Game]] // FIXME 戻り値要検討
  def store(game: Game): F[Option[GameId]]
  def delete(gameId: GameId): F[Option[GameId]]
}

final case class GameSummary(
  gameId: GameId,
  gameState: GameState,
  ownerId: ParticipantId,
  ownerName: ParticipantName,
  challengerId: Option[ParticipantId],
  challengerName: Option[ParticipantName]
) {
  def firstEntry: GameEntry = GameEntry(ownerId, ownerName, Black)
  def secondEntry: Option[GameEntry] =
    challengerId.map(id => GameEntry(id, challengerName.getOrElse(ParticipantName.noName), White))
  def allParticipantIds: List[ParticipantId] = (Some(ownerId) :: challengerId :: Nil).flatten
  def isPlaying: Boolean = gameState == Playing
  def isParticipating(participantId: ParticipantId): Boolean =
    ownerId == participantId || challengerId.contains(participantId)
  def resetParticipantNames(pm: Map[ParticipantId, Participant]): GameSummary = {
    copy(
      ownerName = pm.get(ownerId).map(_.name).getOrElse(ParticipantName.noName),
      challengerName = challengerId.flatMap(id => pm.get(id).map(_.name))
    )
  }
}
object GameSummary {
  def singlePlay: GameSummary = GameSummary(
    GameId(0),
    Playing,
    ParticipantId(-1), ParticipantName("あなた"),
    Some(ParticipantId(-2)), Some(ParticipantName("AI"))
  )
}

sealed trait ServiceError
case object DecodeError extends ServiceError
case object ParticipantNotFound extends ServiceError
case object GameNotFound extends ServiceError
case object OwnedGameExists extends ServiceError
case object EntryFailed extends ServiceError
case object GameStarted extends ServiceError
case object PutError extends ServiceError
case object PassError extends ServiceError
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
  def findByIds(participantIds: Seq[ParticipantId]): F[Map[ParticipantId, Participant]]
  def store(participant: Participant): F[ParticipantId]
}

final case class ParticipateRequest(name: ParticipantName)
final case class CancelGameRequest(gameId: GameId, participantId: ParticipantId)
final case class EntryRequest(gameId: GameId, challengerId: ParticipantId)
final case class StartRequest(gameId: GameId, ownerId: ParticipantId)
final case class PutStoneRequest(gameId: GameId, participantId: ParticipantId, pos: Pos)
final case class PassRequest(gameId: GameId, participantId: ParticipantId)
final case class GiveUpRequest(gameId: GameId, participantId: ParticipantId)

// DEBUG
final case class CreateCustomGameRequest(participantId: ParticipantId, board: Board)

sealed trait GameEvent {
  val gameId: GameId
}
final case class StonePut(gameId: GameId, participantId: ParticipantId, pos: Pos, version: GameVersion) extends GameEvent
final case class GivenUp(gameId: GameId, version: GameVersion) extends GameEvent
final case class Terminated(gameId: GameId, version: GameVersion) extends GameEvent
final case class GamePrepared(gameId: GameId, challengerId: ParticipantId) extends GameEvent
final case class GameStarted(gameId: GameId) extends GameEvent
final case class GameCanceled(gameId: GameId) extends GameEvent

trait GameQuery[F[_]] {
  def gameDetail(gameId: GameId): F[Option[GameDetail]]
}

final case class GameDetail(game: Game, ownerName: ParticipantName, challengerName: Option[ParticipantName]) {
  def firstEntry: GameEntry = GameEntry(game.ownerId, ownerName, Black)
  def secondEntry: Option[GameEntry] =
    game.challengerId.map(id => GameEntry(id, challengerName.getOrElse(ParticipantName.noName), White))
}
object GameDetail {
  def singlePlay: GameDetail = GameDetail(Game.singlePlay, ParticipantName.you, Some(ParticipantName.ai))
}