package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import othello.core.Game
import othello.service.{Service, StonePut, GivenUp}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object RootComponent {

  final case class Props private(
    service: Service[Future],
    eventSourceConnection: EventSourceConnection) {
    @inline def render: VdomElement = Component(this)
  }

  final case class State(rootModel: RootModel) {
    def modRoot(f: RootModel => RootModel): State = copy(f(rootModel))
    def modAppState(f: AppState => AppState): State = modRoot(root => root.copy(appState = f(root.appState)))
    def modGame(f: Game => Game): State = modAppState {
      case PlayingGame(participantId, gameId, game, eventSourceConnection) =>
        PlayingGame(participantId, gameId, f(game), eventSourceConnection)
      case _ => this.rootModel.appState
    }
  }

  object State {
    def init: State = State(RootModel(Initializing))
  }

  final class Backend(bs: BackendScope[Props, State]) {
    val act: Action => Callback = {
      case Participate =>
        bs.withServiceAsync { service =>
          for {
            participantId <- AsyncCallback.fromFuture(service.participate)
            _ <- act(LoadGames(participantId)).asAsyncCallback
          } yield ()
        }.toCallback
      case LoadGame(gameId, participantId) =>
        bs.withProps { props =>
          AsyncCallback.fromFuture(props.service.game(gameId))
            .flatMap {
              case Some(game) =>
                bs.modState(_.modAppState(_ =>
                  PlayingGame(
                    participantId,
                    gameId,
                    game,
                    props.eventSourceConnection)
                )).asAsyncCallback
              case _ => act(LoadGames(participantId)).asAsyncCallback
            }
            .toCallback
        }
      case LoadGames(participantId) =>
        bs.withService { service =>
          AsyncCallback.fromFuture(service.allGames(participantId))
            .flatMap(games => bs.modAppState(_ => Entrance(participantId, games)).asAsyncCallback)
            .toCallback
        }
      case CreateGame(participantId) => bs.props.flatMap(p =>
        AsyncCallback
          .fromFuture(p.service.createGame(participantId))
          .flatMap {
            case Right(gameSummary) =>
              act(LoadGame(gameSummary.gameId, participantId)).asAsyncCallback
            case _ =>
              act(LoadGames(participantId)).asAsyncCallback
          }.toCallback)
      case EntryGame(gameId, participantId) => bs.withService { service =>
        (AsyncCallback.fromFuture(service.entry(gameId, participantId))
          >> act(LoadGame(gameId, participantId)).asAsyncCallback).toCallback
      }
      case PutStone(gameId, participantId, pos) => bs.withService { service =>
        AsyncCallback.fromFuture(service.putStone(gameId, participantId, pos))
          .flatMap {
            case Right(game) => bs.modState(_.modGame(_ => game)).asAsyncCallback
            case _ => Callback().asAsyncCallback
          }
          .toCallback
      }
      case BackToEntrance(participantId) => act(LoadGames(participantId))
      // TODO version対応
      case ReceiveEvent(event) =>
        event match {
          case StonePut(participantId, pos, version) =>
            bs.modState(_.modGame(game => game.putStone(participantId, pos).getOrElse(game)))
          case GivenUp(version) =>
            bs.modState(_.modGame(_.giveUp))
        }
      case GiveUp(gameId, participantId) => bs.withService { service =>
        (for {
          e <- AsyncCallback.fromFuture(service.giveUp(gameId, participantId))
          _ <- e.fold(_ => act(LoadGame(gameId, participantId)), game => bs.modState(_.modGame(_ => game))).asAsyncCallback
        } yield ()).toCallback
      }
    }
    def render(p: Props, s: State): VdomElement =
      <.div(
        s.rootModel.appState match {
          case Initializing => "Loading..."
          case Loading(_) => "Loading..."
          case Entrance(participantId, games) => EntranceComponent.Props(participantId, games, act).render
          case PlayingGame(participantId, gameId, game, eventSourceConnection) =>
            GameComponent.Props(participantId, gameId, game, eventSourceConnection, act).render
        }
      )
  }

  implicit class BackendScopeWrap(bs: BackendScope[Props, State]) {
    def withProps(f: Props => Callback): Callback = bs.props.flatMap(f(_))
    def withAppState(f: AppState => Callback): Callback = bs.state.map(_.rootModel.appState) >>= f
    def withService(f: Service[Future] => Callback): Callback = bs.props.map(_.service) >>= f
    def withServiceAsync[A](f: Service[Future] => AsyncCallback[A]): AsyncCallback[A] =
      bs.props.map(_.service).asAsyncCallback >>= f
    def modAppState(f: AppState => AppState): Callback = bs.modState(_.modAppState(f))
  }

  val Component = ScalaComponent.builder[Props]("RootComponent")
    .initialState(State.init)
    .renderBackend[Backend]
    .componentWillMount {
      _.backend.act(Participate)
    }
    .build
}
