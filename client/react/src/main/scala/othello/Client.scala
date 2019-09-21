package othello

import org.scalajs.dom
import org.scalajs.dom.raw.{EventSource, MessageEvent}
import othello.core.{Game, ParticipantId, Pos}
import othello.service.{GameEvent, GameId, GameSummary}

object Client extends codec.Codec {
  def main(args: Array[String]): Unit = {
    val root = RootComponent.Component(
      RootComponent.Props(
        new OnlineService,
        EventSourceConnection("/events/")))
    root.renderIntoDOM(dom.document.getElementById("stage"))
  }
}

trait EventSourceConnection {
  val baseUrl: String
  private var eventSource: Option[EventSource] = None
  def open(path: String, callback: MessageEvent => Unit): EventSourceConnection = eventSource match {
    case None =>
      val es = new EventSource(baseUrl + path)
      es.onmessage = callback(_)
      eventSource = Some(es)
      this
    case Some(_) => this
  }
  def close: EventSourceConnection = eventSource match {
    case Some(es) =>
      es.close()
      eventSource = None
      this
    case _ => this
  }
}
object EventSourceConnection {
  def apply(endpointBaseUrl: String): EventSourceConnection = new EventSourceConnection {
    override val baseUrl: String = endpointBaseUrl
  }
}

final case class RootModel(appState: AppState)

sealed trait AppState
case object Initializing extends AppState

sealed trait AuthenticatedAppState extends AppState {
  val participantId: ParticipantId
}
final case class Loading(participantId: ParticipantId) extends AuthenticatedAppState
final case class Entrance(participantId: ParticipantId, games: Seq[GameSummary]) extends AuthenticatedAppState
final case class PlayingGame(
  participantId: ParticipantId,
  gameId: GameId,
  game: Game,
  eventSourceConnection: EventSourceConnection
) extends AuthenticatedAppState

sealed trait Action
case object Participate extends Action
final case class LoadGames(participantId: ParticipantId) extends Action
final case class LoadGame(gameId: GameId, participantId: ParticipantId) extends Action
final case class CreateGame(participantId: ParticipantId) extends Action
final case class EntryGame(gameId: GameId, participantId: ParticipantId) extends Action

sealed trait GameAction extends Action
final case class PutStone(gameId: GameId, participantId: ParticipantId, pos: Pos) extends GameAction
final case class GiveUp(gameId: GameId, participantId: ParticipantId) extends GameAction
final case class ReceiveEvent(event: GameEvent) extends GameAction
