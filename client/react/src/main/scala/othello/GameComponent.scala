package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.MessageEvent
import othello.core._
import othello.service.{GameEvent, GameId, StonePut}

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
    def render(p: Props): VdomElement = {
      import p.game.othello
      <.div(
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
                  ^.onClick --> p.handler(PutStone(p.gameId, p.participantId, Pos(x, y))),
                  stone.fold("")(_ => "●")
                )
              }.toTagMod
            )
          }.toTagMod
        )),
        // TODO: GiveUpボタンををつける
        // TODO: 決着後のステートに対応する
        ScoreView.Props(p.game.othello.score).render
      )
    }
    def initialize(): Callback = $.props.map { p =>
      p.eventSourceConnection.open(p.gameId.value.toString, handleMessageEvent)
      ()
    }
    def handleMessageEvent(messageEvent: MessageEvent): Unit = {
      import io.circe.parser._
      import io.circe.generic.auto._
      decode[GameEvent](messageEvent.data.toString).foreach {
        case e: StonePut =>
          $.props.flatMap(_.handler(ReceiveEvent(e))).runNow()
        case _ =>
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
        s"Black: ${p.score(Black)} - White: ${p.score(White)}"
      )
  }

  val Component = ScalaComponent.builder[Props]("ScoreView")
    .renderBackend[Backend]
    .build
}
