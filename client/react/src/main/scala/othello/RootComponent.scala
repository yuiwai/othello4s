package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import othello.GameComponent.GameSettings
import othello.core.{Game, ParticipantName}
import othello.service._

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
    def modGlobalState(f: GlobalState => GlobalState): State = modRoot(root => root.copy(globalState = f(root.globalState)))
    def modGame(f: Game => Game): State = modAppState {
      case PlayingGame(participantId, gameId, game, eventSourceConnection) =>
        PlayingGame(participantId, gameId, f(game), eventSourceConnection)
      case _ => this.rootModel.appState
    }
    def modGameSettings(f: GameSettings => GameSettings): State =
      modGlobalState(gs => gs.copy(gameSettings = f(gs.gameSettings)))
  }

  object State {
    def init: State = State(RootModel(GlobalState(GameSettings()), Initializing))
  }

  final class Backend(bs: BackendScope[Props, State]) {
    val act: Action => Callback = {
      case Participate =>
        bs.withServiceAsync { service =>
          for {
            participantId <- AsyncCallback.fromFuture(service.participate(ParticipantName.noName))
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
            case Right(game) =>
              bs.modState(_.modGame(_ => game)).asAsyncCallback
            case _ => Callback().asAsyncCallback
          }
          .toCallback
      }
      case Pass(gameId, participantId) => bs.withProps { props =>
        AsyncCallback.fromFuture(props.service.pass(gameId, participantId))
          .flatMap {
            case Right(game) =>
              bs.modState(_.modAppState(_ =>
                PlayingGame(
                  participantId,
                  gameId,
                  game,
                  props.eventSourceConnection)
              )).asAsyncCallback
            case _ => Callback.empty.asAsyncCallback
          }
          .toCallback
      }
      case BackToEntrance(participantId) => act(LoadGames(participantId))
      case ReceiveEvent(currentParticipantId, event) =>
        event match {
          case StonePut(gameId, participantId, pos, version) =>
            bs.withGame(_.filter(_.canAcceptVersion(version))
              .fold(act(LoadGame(gameId, currentParticipantId))) { game =>
                bs.putGame(game.putStone(participantId, pos).getOrElse(game))
              })
          case GivenUp(gameId, version) =>
            bs.withGame(_.filter(_.canAcceptVersion(version))
              .fold(act(LoadGame(gameId, currentParticipantId))) { game =>
                bs.putGame(game.giveUp)
              })
          case Terminated(_, _) =>
            // FIXME versionのチェックができるか？(TerminatedされたGameはすでに削除されている可能性がある)
            bs.withProps(p => Callback(p.eventSourceConnection.close))
          case GameStarted(_, _) =>
            bs.modState(_.modGame(_.start))
        }
      case GiveUp(gameId, participantId) => bs.withService { service =>
        (for {
          e <- AsyncCallback.fromFuture(service.giveUp(gameId, participantId))
          _ <- e.fold(_ => act(LoadGame(gameId, participantId)), game => bs.modState(_.modGame(_ => game))).asAsyncCallback
        } yield ()).toCallback
      }

      case UpdateGameSettings(settings) =>
        bs.modState(_.modGameSettings(_ => settings))

      // DEBUG
      case BeginEditMode(participantId) => bs.modState(_.modAppState(_ => Edit(participantId)))
      case CreateCustomGame(participantId, board) =>
        bs.withProps { p =>
          AsyncCallback.fromFuture(p.service.createCustomGame(participantId, board))
            .flatMap {
              case Some(gameId) => act(LoadGame(gameId, participantId)).asAsyncCallback
              case None => Callback.empty.asAsyncCallback
            }
            .toCallback
        }
    }
    def render(p: Props, s: State): VdomElement =
      <.div(
        s.rootModel.appState match {
          case Initializing => "Loading..."
          case Loading(_) => "Loading..."
          case Entrance(participantId, games) => EntranceComponent.Props(participantId, games, act).render
          case PlayingGame(participantId, gameId, game, eventSourceConnection) =>
            GameComponent.Props(
              participantId,
              gameId,
              game,
              s.rootModel.globalState.gameSettings,
              eventSourceConnection, act).render
          case Edit(participantId) => EditComponent.Props(participantId, act).render
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
    def withGame(f: Option[Game] => Callback): Callback = bs.state.flatMap(_.rootModel.appState match {
      case gas: GameAppState => f(Some(gas.game))
      case _ => f(None)
    })
    def putGame(game: Game): Callback = modAppState {
      case gas: GameAppState => gas.putGame(game)
      case other => other
    }
  }

  val Component = ScalaComponent.builder[Props]("RootComponent")
    .initialState(State.init)
    .renderBackend[Backend]
    .componentWillMount {
      _.backend.act(Participate)
    }
    .build
}
