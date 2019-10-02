package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import othello.GameComponent.GameSettings
import othello.core.{Game, ParticipantId}
import othello.service._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object RootComponent {

  final case class Props private(
    serviceSet: ServiceSet,
    eventSourceConnection: EventSourceConnection) {
    @inline def render: VdomElement = Component(this)
    def service(networkMode: NetworkMode): Service[Future] = networkMode match {
      case Online => serviceSet.onlineService
      case Offline => serviceSet.offlineService
    }
  }

  sealed trait NetworkMode
  case object Online extends NetworkMode
  case object Offline extends NetworkMode

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
    def modNetworkMode(f: NetworkMode => NetworkMode): State = modGlobalState(gs => gs.copy(networkMode = f(gs.networkMode)))
  }

  object State {
    def init: State = State(RootModel(GlobalState(GameSettings(), Online), Initializing))
  }

  final class Backend(bs: BackendScope[Props, State]) {
    val act: Action => Callback = {
      case CompositeAction(first, second) => (act(first).async >> act(second).async).toCallback
      case Initialize => bs.modAppState(_ => Initializing)
      case Participate(name) =>
        bs.withServiceAsync { service =>
          for {
            participantId <- AsyncCallback.fromFuture(service.participate(name))
            _ <- act(LoadGames(participantId)).asAsyncCallback
          } yield ()
        }.toCallback
      case LoadGame(gameId, participantId) =>
        bs.withService { service =>
          bs.withProps { props =>
            AsyncCallback.fromFuture(service.game(gameId))
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
        }
      case LoadGames(participantId) =>
        bs.withService { service =>
          AsyncCallback.fromFuture(service.allGames(participantId))
            .flatMap(games => bs.modAppState(_ => Entrance(participantId, games)).asAsyncCallback)
            .toCallback
        }
      case CreateGame(participantId) => bs.withService { service =>
        bs.props.flatMap(p =>
          AsyncCallback
            .fromFuture(service.createGame(participantId))
            .flatMap {
              case Right(gameSummary) =>
                act(LoadGame(gameSummary.gameId, participantId)).asAsyncCallback
              case _ =>
                act(LoadGames(participantId)).asAsyncCallback
            }.toCallback)
      }
      case StartGame(gameId, participantId) => bs.withService { service =>
        AsyncCallback.fromFuture(service.start(gameId, participantId)).toCallback
      }
      case CancelGame(gameId, participantId) => bs.withService { service =>
        (AsyncCallback
          .fromFuture(service.cancel(gameId, participantId)) >>
          act(LoadGames(participantId)).async).toCallback
      }
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
      case Pass(gameId, participantId) => bs.withService { service =>
        bs.withProps { props =>
          AsyncCallback.fromFuture(service.pass(gameId, participantId))
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
          case GamePrepared(gameId, challengerId) =>
            bs.withGame {
              _.flatMap(_.entry(challengerId).toOption)
                .fold(Callback.empty)(bs.putGame)
            }
          case GameStarted(_) =>
            bs.withGame {
              _.flatMap(_.start.toOption)
                .fold(Callback.empty)(bs.putGame)
            }
          case GameCanceled(gameId) =>
            bs.withGame {
              _.flatMap(_.cancel)
                .fold(Callback.empty)(bs.putGame)
            }
        }
      case GiveUp(gameId, participantId) => bs.withService { service =>
        (for {
          e <- AsyncCallback.fromFuture(service.giveUp(gameId, participantId))
          _ <- e.fold(_ => act(LoadGame(gameId, participantId)), game => bs.modState(_.modGame(_ => game))).asAsyncCallback
        } yield ()).toCallback
      }

      case UpdateGameSettings(settings) =>
        bs.modState(_.modGameSettings(_ => settings))

      case StartOnlineMode =>
        bs.modState(_.modGlobalState(_.toOnline))
      case StartOfflineMode =>
        bs.modState(_.modAppState(_ => OfflineGame()).modGlobalState(_.toOffline))

      // DEBUG
      case BeginEditMode(participantId) => bs.modState(_.modAppState(_ => Edit(participantId)))
      case CreateCustomGame(participantId, board) =>
        bs.withService { service =>
          AsyncCallback.fromFuture(service.createCustomGame(participantId, board))
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
          case Initializing => TopComponent.Props(act).render
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
          case OfflineGame() => OfflineGameComponent.Props(act).render
        }
      )
  }

  implicit class BackendScopeWrap(bs: BackendScope[Props, State]) {
    def withProps(f: Props => Callback): Callback = bs.props.flatMap(f(_))
    def withGlobalState(f: GlobalState => Callback): Callback = bs.state.map(_.rootModel.globalState) >>= f
    def withGlobalStateAsync[A](f: GlobalState => AsyncCallback[A]): AsyncCallback[A] =
      bs.state.map(_.rootModel.globalState).asAsyncCallback >>= f
    def withAppState(f: AppState => Callback): Callback = bs.state.map(_.rootModel.appState) >>= f
    def withService(f: Service[Future] => Callback): Callback =
      bs.withGlobalState(gs => bs.props.map(_.service(gs.networkMode)) >>= f)
    def withServiceAsync[A](f: Service[Future] => AsyncCallback[A]): AsyncCallback[A] =
      bs.withGlobalStateAsync(gs => bs.props.map(_.service(gs.networkMode)).asAsyncCallback >>= f)
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
    .build
}
