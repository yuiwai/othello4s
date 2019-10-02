package othello

import org.scalajs.dom
import org.scalajs.dom.raw.{EventSource, MessageEvent}
import othello.GameComponent.GameSettings
import othello.RootComponent.{NetworkMode, Offline, Online}
import othello.core._
import othello.service.{GameEvent, GameId, GameSummary, Service}

import scala.concurrent.Future

object Client extends codec.Codec {
  def main(args: Array[String]): Unit = setup()
  def setup(): Unit = {
    val root = RootComponent.Component(
      RootComponent.Props(
        ServiceSet(new OnlineService, new OfflineService),
        EventSourceConnection("/events/")))
    root.renderIntoDOM(dom.document.getElementById("stage"))
  }
}

final case class ServiceSet(onlineService: Service[Future], offlineService: Service[Future])

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

final case class RootModel(globalState: GlobalState, appState: AppState)

final case class GlobalState(gameSettings: GameSettings, networkMode: NetworkMode) {
  def toOnline: GlobalState = copy(networkMode = Online)
  def toOffline: GlobalState = copy(networkMode = Offline)
}

sealed trait AppState
case object Initializing extends AppState

sealed trait AuthenticatedAppState extends AppState {
  val participantId: ParticipantId
}
final case class Loading(participantId: ParticipantId) extends AuthenticatedAppState
final case class Entrance(participantId: ParticipantId, games: Seq[GameSummary]) extends AuthenticatedAppState
sealed trait GameAppState extends AuthenticatedAppState {
  val game: Game
  def putGame(game: Game): GameAppState
}
final case class PlayingGame(
  participantId: ParticipantId,
  gameId: GameId,
  game: Game,
  eventSourceConnection: EventSourceConnection
) extends GameAppState {
  override def putGame(game: Game): GameAppState = copy(game = game)
}
final case class Edit(
  participantId: ParticipantId
) extends AuthenticatedAppState
final case class OfflineGame() extends AppState

sealed trait Action {
  def >>(action: Action): CompositeAction = CompositeAction(this, action)
}

case object Initialize extends Action
final case class Participate(name: ParticipantName) extends Action
final case class LoadGames(participantId: ParticipantId) extends Action
final case class LoadGame(gameId: GameId, participantId: ParticipantId) extends Action
final case class CreateGame(participantId: ParticipantId) extends Action
final case class CreateCustomGame(participantId: ParticipantId, board: Board) extends Action
final case class EntryGame(gameId: GameId, participantId: ParticipantId) extends Action
final case class BeginEditMode(participantId: ParticipantId) extends Action
case object StartOnlineMode extends Action
case object StartOfflineMode extends Action
final case class CompositeAction(first: Action, second: Action) extends Action

sealed trait GameAction extends Action
final case class StartGame(gameId: GameId, participantId: ParticipantId) extends GameAction
final case class CancelGame(gameId: GameId, participantId: ParticipantId) extends GameAction
final case class PutStone(gameId: GameId, participantId: ParticipantId, pos: Pos) extends GameAction
final case class Pass(gameId: GameId, participantId: ParticipantId) extends GameAction
final case class GiveUp(gameId: GameId, participantId: ParticipantId) extends GameAction
final case class ReceiveEvent(participantId: ParticipantId, event: GameEvent) extends GameAction
final case class BackToEntrance(participantId: ParticipantId) extends GameAction
final case class UpdateGameSettings(settings: GameSettings) extends GameAction
