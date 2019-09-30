package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.MessageEvent
import othello.GameComponent.GameSettings
import othello.core._
import othello.service.{GameEvent, GameId}

object GameComponent {
  final case class Props(
    participantId: ParticipantId,
    gameId: GameId,
    game: core.Game,
    settings: GameSettings,
    eventSourceConnection: EventSourceConnection,
    handler: GameAction => Callback) {
    @inline def render: VdomElement = Component(this)
  }

  final case class GameSettings(
    useHint: Boolean = false
  ) {
    def tglUseHint: GameSettings = copy(useHint = !useHint)
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def backToEntranceButton(p: Props): VdomElement = {
      <.button(
        ^.onClick --> p.handler(BackToEntrance(p.participantId)),
        "ゲーム一覧に戻る"
      )
    }
    def withIfWatchingMode(p: Props)(vdomElement: => VdomElement): VdomNode = {
      if (p.game.mode(p.participantId) == WatchingMode) vdomElement
      else EmptyVdom
    }
    def render(p: Props): VdomElement = {
      <.div(
        GameInformationBar.render(
          p.game,
          p.participantId,
          () => p.handler(Pass(p.gameId, p.participantId)),
          () => p.handler(GiveUp(p.gameId, p.participantId)),
          () => p.handler(CancelGame(p.gameId, p.participantId)),
          backToEntranceButton(p)
        ),
        BoardComponent.Props(p.game.othello.board, { pos =>
          p.game.mode(p.participantId) match {
            case PlayerMode =>
              if (p.game.putStone(p.participantId, pos).isRight)
                p.handler(PutStone(p.gameId, p.participantId, pos))
              else Callback.empty
            case _ => Callback.empty
          }
        }, if (p.settings.useHint) p.game.othello.canPutAll.map(arr => arr.pos) else _ => false).render,
        ScoreView.Props(p.game.othello.score).render,
        SettingsView.Props(p.settings, settings => p.handler(UpdateGameSettings(settings))).render
      )
    }
    def initialize(): Callback = $.props.map { p =>
      p.eventSourceConnection.open(p.gameId.value.toString, handleMessageEvent)
      ()
    }
    def handleMessageEvent(messageEvent: MessageEvent): Unit = {
      import io.circe.generic.auto._
      import io.circe.parser._
      decode[GameEvent](messageEvent.data.toString).foreach {
        e => $.props.flatMap(p => p.handler(ReceiveEvent(p.participantId, e))).runNow()
      }
    }
  }

  val Component = ScalaComponent.builder[Props]("GameComponent")
    .renderBackend[Backend]
    .componentWillMount {
      _.backend.initialize()
    }
    .build
}

object ScoreView {

  final case class Props(score: Map[StoneColor, Int]) {
    @inline def render: VdomElement = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomElement =
      <.div(
        s"黒: ${p.score(Black)} - 白: ${p.score(White)}"
      )
  }

  val Component = ScalaComponent.builder[Props]("ScoreView")
    .renderBackend[Backend]
    .build
}

object SettingsView {

  final case class Props(settings: GameSettings, updater: GameSettings => Callback) {
    @inline def render: VdomElement = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomElement =
      <.div(
        <.button(
          ^.onClick --> p.updater(p.settings.tglUseHint),
          s"候補手表示: " + (if (p.settings.useHint) "On" else "Off")
        )
      )
  }

  val Component = ScalaComponent.builder[Props]("SettingsView")
    .renderBackend[Backend]
    .build
}

object GameInformationBar {
  def render(
    game: Game,
    participantId: ParticipantId,
    passHandler: () => Callback,
    giveUpHandler: () => Callback,
    cancelHandler: () => Callback,
    exitView: => VdomNode): TagMod =
    game.state match {
      case Terminated(winner) =>
        game.mode(participantId) match {
          case WatchingMode =>
            <.div(
              "ゲーム終了",
              exitView
            )
          case PlayerMode =>
            <.div(
              winner match {
                case Some(`participantId`) => "あなたの勝ちです!"
                case None => "引き分けです"
                case _ => "あなたの負けです..."
              },
              exitView
            )
        }
      case Waiting =>
        <.div(
          "参加者を待っています...",
          <.button(
            ^.onClick --> cancelHandler(),
            "Cancel"
          ),
        )
      case Playing =>
        game.mode(participantId) match {
          case WatchingMode => <.div(
            "観戦中です",
            exitView
          )
          case PlayerMode =>
            PlayingMenuBar.render(
              game,
              participantId,
              passHandler,
              giveUpHandler,
              exitView
            )
        }
      case _ => TagMod.empty
    }
}

object PlayingMenuBar {
  def render(
    game: Game,
    playerId: ParticipantId,
    passHandler: () => Callback,
    giveUpHandler: () => Callback,
    watchingModeView: => VdomNode
  ): VdomElement =
    if (game.isTurnOf(playerId)) {
      <.div(
        "あなたの番です",
        if (game.othello.canNotPut) {
          <.button(
            ^.onClick --> passHandler(),
            "パス"
          )
        } else TagMod.empty,
        <.button(
          ^.onClick --> giveUpHandler(),
          "投了"
        )
      )
    } else {
      <.div(
        "相手の番です...",
        withIfWatchingMode(game, playerId)(watchingModeView)
      )
    }
  def withIfWatchingMode(game: Game, playerId: ParticipantId)(vdomNode: => VdomNode): VdomNode = {
    if (game.mode(playerId) == WatchingMode) vdomNode
    else EmptyVdom
  }
}