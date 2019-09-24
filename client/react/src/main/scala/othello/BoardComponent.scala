package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import othello.core.{Board, Pos}

object BoardComponent {

  final case class Props(board: Board, handler: Pos => Callback) {
    @inline def render: VdomElement = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomElement =
      <.table(<.tbody(
        (1 to 8).map { y =>
          <.tr(
            (1 to 8).map { x =>
              val stone = p.board.get(Pos(x, y))
              <.td(
                // TODO Styleは切り出したい
                ^.color := stone.fold("black")(_.toString),
                ^.borderSpacing := "1px",
                ^.backgroundColor := "green",
                ^.textAlign := "center",
                ^.width := "30px",
                ^.height := "30px",
                ^.cursor := "pointer",
                ^.onClick --> p.handler(Pos(x, y)),
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
