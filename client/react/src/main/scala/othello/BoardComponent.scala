package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object BoardComponent {

  final case class Props() {
    @inline def render: VdomElement = Component(this)
  }


  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomElement =
      <.div
  }

  val Component = ScalaComponent.builder[Props]("BoardComponent")
    .renderBackend[Backend]
    .build
}
