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
  private val headers = Map("content-type" -> "application/json")
  def post[R: io.circe.Decoder](url: String, json: Json = Json.Null): Future[Either[io.circe.Error, R]] =
    Ajax.post(url, json.noSpaces, headers = headers)
      .map[Either[io.circe.Error, R]](r => decode[R](r.responseText))
  override def participate(name: ParticipantName): Future[ParticipantId] =
    post[ParticipantId]("/participants/create", ParticipateRequest(ParticipantName.noName).asJson)
      .map(_.getOrElse(ParticipantId(0)))
  override def allGames(participantId: ParticipantId): Future[Seq[GameSummary]] =
    Ajax
      .post("/games", participantId.asJson.noSpaces, headers = headers)
      .map(r => decode[Seq[GameSummary]](r.responseText).getOrElse(Seq.empty))
  override def game(gameId: GameId): Future[Option[Game]] =
    Ajax.get(s"/games/${gameId.value}")
      .map(r => decode[Option[Game]](r.responseText)
        .fold(_ => None, identity)
      )
  override def createGame(participantId: ParticipantId): Future[Either[ServiceError, GameSummary]] =
    Ajax
      .post("/games/create", participantId.asJson.noSpaces, headers = headers)
      .map(r =>
        decode[Either[ServiceError, GameSummary]](r.responseText)
          .fold[Either[ServiceError, GameSummary]](_ => Left(DecodeError), identity)
      )
  def entry(gameId: GameId, participantId: ParticipantId): Future[Either[ServiceError, EntryId]] =
    Ajax
      .post("/games/entry", EntryRequest(gameId, participantId).asJson.noSpaces, headers = headers)
      .map(r =>
        decode[Either[ServiceError, EntryId]](r.responseText)
          .fold(_ => {
            Left(DecodeError)
          }, identity)
      )
  def putStone(gameId: GameId, participantId: ParticipantId, pos: Pos): Future[Either[ServiceError, Game]] =
    Ajax
      .post("/games/putStone", PutStoneRequest(gameId, participantId, pos).asJson.noSpaces, headers = headers)
      .map { r =>
        decode[Either[ServiceError, Game]](r.responseText)
          .fold(_ => {
            Left(DecodeError)
          }, identity)
      }
  override def pass(gameId: GameId, participantId: ParticipantId): Future[Either[ServiceError, Game]] =
    Ajax
      .post("/games/pass", PassRequest(gameId, participantId).asJson.noSpaces, headers = headers)
      .map { r =>
        decode[Either[ServiceError, Game]](r.responseText)
          .fold(_ => {
            Left(DecodeError)
          }, identity)
      }

  override def giveUp(gameId: GameId, participantId: ParticipantId): Future[Either[ServiceError, Game]] =
    Ajax
      .post("/games/giveUp", GiveUpRequest(gameId, participantId).asJson.noSpaces, headers = headers)
      .map { r =>
        decode[Either[ServiceError, Game]](r.responseText)
          .fold(_ => Left(DecodeError), identity)
      }

  // DEBUG
  override def createCustomGame(participantId: ParticipantId, board: Board): Future[Option[GameId]] =
    post[Option[GameId]]("/games/createCustom", CreateCustomGameRequest(participantId, board).asJson)
      .map {
        case Left(_) => None
        case Right(gameId) => gameId
      }
}

/*
class OfflineService extends Service[Future] {
  override def allGames: Future[Seq[GameId]] = ???
  override def game(gameId: GameId): Future[Option[Game]] = ???
}
*/

/*
class InMemoryGameRepository extends GameRepository {
}
 */
