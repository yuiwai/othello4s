package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import othello.core.{Board, Pos}

object BoardComponent {

  final case class Props(board: Board, handler: Pos => Callback, effect: Pos => Boolean = { _ => false }) {
    @inline def render: VdomElement = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomElement =
      <.table(<.tbody(
        (1 to 8).map { y =>
          <.tr(
            (1 to 8).map { x =>
              val pos = Pos(x, y)
              val stone = p.board.get(pos)
              <.td(
                // TODO Styleは切り出したい
                ^.color := stone.fold("black")(_.toString),
                ^.borderSpacing := "1px",
                // TODO 暫定で、マスの背景色を変える機能
                ^.backgroundColor := (if (p.effect(pos)) "skyblue" else "green"),
                ^.textAlign := "center",
                ^.width := "30px",
                ^.height := "30px",
                ^.cursor := "pointer",
                ^.onClick --> p.handler(pos),
                stone.fold("")(_ => "●")
              )
            }.toTagMod
          )
        }.toTagMod
      ))
  }

  def Component = ScalaComponent.builder[Props]("BoardComponent")
    .renderBackend[Backend]
    .build
}
