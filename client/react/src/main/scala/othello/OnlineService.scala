package othello

import io.circe.Json
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom.ext.Ajax
import othello.core._
import othello.service._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class OnlineService extends Service[Future] with codec.Codec {
  type WithServiceError[T] = Either[ServiceError, T]
  private val headers = Map("content-type" -> "application/json")
  def get[R: io.circe.Decoder](url: String): Future[Either[io.circe.Error, R]] =
    Ajax.get(url)
      .map[Either[io.circe.Error, R]](r => decode[R](r.responseText))
  def post[R: io.circe.Decoder](url: String, json: Json = Json.Null): Future[Either[io.circe.Error, R]] =
    Ajax.post(url, json.noSpaces, headers = headers)
      .map[Either[io.circe.Error, R]](r => decode[R](r.responseText))
  override def participate(name: ParticipantName): Future[ParticipantId] =
    post[ParticipantId]("/participants/create", ParticipateRequest(name).asJson)
      .map(_.getOrElse(ParticipantId(0)))
  override def allGames(participantId: ParticipantId): Future[Seq[GameSummary]] =
    post[Seq[GameSummary]]("/games", participantId.asJson)
      .map(_.getOrElse(Seq.empty))
  override def game(gameId: GameId): Future[Option[Game]] =
    get[Option[Game]](s"/games/${gameId.value}")
      .map(_.fold(_ => None, identity))
  override def createGame(participantId: ParticipantId): Future[Either[ServiceError, GameSummary]] =
    post[WithServiceError[GameSummary]]("/games/create", participantId.asJson)
      .map(_.fold[Either[ServiceError, GameSummary]](_ => Left(DecodeError), identity))
  override def cancel(gameId: GameId, participantId: ParticipantId): Future[Option[ServiceError]] =
    post[Option[ServiceError]]("/games/cancel", CancelGameRequest(gameId, participantId).asJson)
      .map(_.fold(_ => Some(DecodeError), identity))
  override def entry(gameId: GameId, participantId: ParticipantId): Future[Either[ServiceError, EntryId]] =
    post[WithServiceError[EntryId]]("/games/entry", EntryRequest(gameId, participantId).asJson)
      .map(_.fold(_ => Left(DecodeError), identity))
  override def start(gameId: GameId, ownerId: ParticipantId): Future[Either[ServiceError, Game]] =
    post[WithServiceError[Game]]("/games/start", StartRequest(gameId, ownerId).asJson)
      .map(_.fold(_ => Left(DecodeError), identity))
  override def putStone(gameId: GameId, participantId: ParticipantId, pos: Pos): Future[Either[ServiceError, Game]] =
    post[WithServiceError[Game]]("/games/putStone", PutStoneRequest(gameId, participantId, pos).asJson)
      .map(_.fold(_ => Left(DecodeError), identity))
  override def pass(gameId: GameId, participantId: ParticipantId): Future[Either[ServiceError, Game]] =
    post[WithServiceError[Game]]("/games/pass", PassRequest(gameId, participantId).asJson)
      .map(_.fold(_ => Left(DecodeError), identity))
  override def giveUp(gameId: GameId, participantId: ParticipantId): Future[Either[ServiceError, Game]] =
    post[WithServiceError[Game]]("/games/giveUp", GiveUpRequest(gameId, participantId).asJson)
      .map(_.fold(_ => Left(DecodeError), identity))

  // DEBUG
  override def createCustomGame(participantId: ParticipantId, board: Board): Future[Option[GameId]] =
    post[Option[GameId]]("/games/createCustom", CreateCustomGameRequest(participantId, board).asJson)
      .map {
        case Left(_) => None
        case Right(gameId) => gameId
      }
}

class OfflineService extends Service[Future] {
  override def participate(name: ParticipantName): Future[ParticipantId] = ???
  override def allGames(participantId: ParticipantId): Future[Seq[GameSummary]] = ???
  override def game(gameId: GameId): Future[Option[Game]] = ???
  override def createGame(participantId: ParticipantId): Future[Either[ServiceError, GameSummary]] = ???
  override def cancel(gameId: GameId, participantId: ParticipantId): Future[Option[ServiceError]] = ???
  override def entry(gameId: GameId, participantId: ParticipantId): Future[Either[ServiceError, EntryId]] = ???
  override def start(gameId: GameId, ownerId: ParticipantId): Future[Either[ServiceError, Game]] = ???
  override def putStone(gameId: GameId, participantId: ParticipantId, pos: Pos): Future[Either[ServiceError, Game]] = ???
  override def pass(gameId: GameId, participantId: ParticipantId): Future[Either[ServiceError, Game]] = ???
  override def giveUp(gameId: GameId, participantId: ParticipantId): Future[Either[ServiceError, Game]] = ???
  override def createCustomGame(participantId: ParticipantId, board: Board): Future[Option[GameId]] = ???
}

/*
class InMemoryGameRepository extends GameRepository {
}
 */
