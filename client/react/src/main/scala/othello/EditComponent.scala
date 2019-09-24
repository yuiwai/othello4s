package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object EditComponent {

  final case class Props() {
    @inline def render: VdomElement = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomElement =
      <.div(
        <.h2("エディットモード")
        // TODO: エディットメニュー
        // TODO: ボード
      )
  }

  val Component = ScalaComponent.builder[Props]("EditComponent")
    .renderBackend[Backend]
    .build
}
