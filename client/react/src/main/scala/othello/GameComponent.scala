package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.MessageEvent
import othello.core._
import othello.service.{GameEvent, GameId}

object GameComponent {
  final case class Props(
    participantId: ParticipantId,
    gameId: GameId,
    game: core.Game,
    eventSourceConnection: EventSourceConnection,
    handler: GameAction => Callback) {
    @inline def render: VdomElement = Component(this)
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
      import p.game.othello
      <.div(
        p.game.state match {
          case Terminated(winner) =>
            p.game.mode(p.participantId) match {
              case WatchingMode =>
                <.div(
                  "ゲーム終了",
                  backToEntranceButton(p)
                )
              case PlayerMode =>
                <.div(
                  winner match {
                    case Some(p.participantId) => "あなたの勝ちです!"
                    case None => "引き分けです"
                    case _ => "あなたの負けです..."
                  },
                  backToEntranceButton(p)
                )
            }
          case Waiting =>
            <.div("参加者を待っています...")
          case Playing =>
            p.game.mode(p.participantId) match {
              case WatchingMode => <.div("観戦中です")
              case PlayerMode =>
                if (p.game.isTurnOf(p.participantId)) {
                  <.div(
                    "あなたの番です",
                    if (othello.canNotPut) {
                      <.button(
                        ^.onClick --> p.handler(Pass(p.gameId, p.participantId)),
                        "パス"
                      )
                    } else TagMod.empty,
                    <.button(
                      ^.onClick --> p.handler(GiveUp(p.gameId, p.participantId)),
                      "投了"
                    )
                  )
                } else {
                  <.div(
                    "相手の番です...",
                    withIfWatchingMode(p)(backToEntranceButton(p))
                  )
                }
            }
          case _ => TagMod.empty
        },
        <.table(<.tbody(
          (1 to 8).map { y =>
            <.tr(
              (1 to 8).map { x =>
                val stone = othello.get(x, y)
                <.td(
                  ^.color := stone.fold("black")(_.toString),
                  ^.borderSpacing := "1px",
                  ^.backgroundColor := "green",
                  ^.textAlign := "center",
                  ^.width := "30px",
                  ^.height := "30px",
                  p.game.mode(p.participantId) match {
                    case PlayerMode =>
                      ^.onClick --> p.handler(PutStone(p.gameId, p.participantId, Pos(x, y)))
                    case _ => TagMod.empty
                  },
                  stone.fold("")(_ => "●")
                )
              }.toTagMod
            )
          }.toTagMod
        )),
        ScoreView.Props(p.game.othello.score).render
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
