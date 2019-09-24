package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import othello.core._

object EditComponent {
  sealed trait EditMode
  case object PutBlack extends EditMode
  case object PutWhite extends EditMode

  final case class Props(
    participantId: ParticipantId,
    handler: Action => Callback) {
    @inline def render: VdomElement = Component(this)
  }

  final case class State(editMode: EditMode, board: Board) {
    def put(pos: Pos): State = copy(board = editMode match {
      case PutBlack => board.mod(pos)(_.fold(Some(Black): Option[StoneColor]) {
        case Black => None
        case White => Some(Black)
      })
      case PutWhite => board.mod(pos)(_.fold(Some(White): Option[StoneColor]) {
        case White => None
        case Black => Some(White)
      })
    })
  }

  final class Backend($: BackendScope[Props, State]) {
    def render(p: Props, s: State): VdomElement =
      <.div(
        <.h2("エディットモード"),
        <.div(
          ^.margin := "5px",
          Map(PutBlack -> "黒", PutWhite -> "白").map {
            case (editMode, label) =>
              <.span(
                ^.backgroundColor := (if (editMode == s.editMode) "skyblue" else "white"),
                ^.onClick --> $.modState(_.copy(editMode = editMode)),
                // TODO Styleは別に切り出したい
                ^.cursor := "pointer",
                ^.margin := "3px",
                ^.padding := "5px",
                label
              )
          }.toTagMod),
        <.div(
          BoardComponent.Props(s.board, pos => $.modState(_.put(pos))).render
        ),
        <.div(
          <.button(
            ^.onClick --> p.handler(LoadGames(p.participantId)),
            "ゲーム一覧に戻る"
          ),
          // TODO: validなboardでのみ実行可能にしたい
          <.button(
            ^.onClick --> p.handler(CreateCustomGame(p.participantId, s.board)),
            "この盤面からゲーム開始"
          )
        )
      )
  }

  val Component = ScalaComponent.builder[Props]("EditComponent")
    .initialState(State(PutBlack, Board()))
    .renderBackend[Backend]
    .build
}
