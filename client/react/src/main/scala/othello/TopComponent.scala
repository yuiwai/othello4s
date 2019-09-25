package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import othello.core.ParticipantName

object TopComponent {

  final case class Props(handler: Action => Callback) {
    @inline def render: VdomElement = Component(this)
  }

  final case class State(name: String)

  final class Backend($: BackendScope[Props, State]) {
    def render(p: Props, s: State): VdomElement =
      <.div(
        <.h2("ネットワークモード選択"),
        <.div(
          <.input(
            ^.onChange ==> { e: ReactEventFromInput =>
              e.persist()
              $.modState(_.copy(name = e.target.value))
            },
            ^.value := s.name,
            ^.placeholder := "名前を入力してください"
          ),
          <.button(
            ^.disabled := s.name.isEmpty,
            ^.onClick --> p.handler(Participate(ParticipantName(s.name))),
            "オンラインモード"
          )
        ),
        <.div(
          <.button(
            "オフラインモード"
          )
        )
      )
  }

  val Component = ScalaComponent.builder[Props]("TopComponent")
    .initialState(State(""))
    .renderBackend[Backend]
    .build
}
