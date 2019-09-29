package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import othello.core.Board

object OfflineGameComponent {

  sealed trait OfflineGameMode

  final case class Props(handler: Action => Callback) {
    @inline def render: VdomElement = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomElement =
      <.div(
        <.div(
          <.button(
            ^.onClick --> p.handler(Initialize), // FIXME
            "1人プレイを開始"
          ),
          <.button(
            "AIと対戦を開始"
          )
        ),
        BoardComponent.Props(Board(), _ => Callback.empty).render,
        <.div(
          <.button(
            ^.onClick --> p.handler(Initialize),
            "トップに戻る"
          )
        )
      )
  }

  val Component = ScalaComponent.builder[Props]("OfflineGameComponent")
    .renderBackend[Backend]
    .build
}
