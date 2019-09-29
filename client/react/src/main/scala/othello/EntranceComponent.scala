package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import othello.core.{ParticipantId, ParticipantName, Waiting}
import othello.service.GameSummary

object EntranceComponent {

  final case class Props(participantId: ParticipantId, games: Seq[GameSummary], handler: Action => Callback) {
    @inline def render: VdomElement = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomElement =
      <.div(
        <.h2("ゲーム一覧"),
        if (p.games.isEmpty) <.div("アクティブなゲームがありません。")
        else p.games.map { g =>
          <.div(
            g.gameState match {
              case Waiting =>
                s"${g.ownerName.value} 【参加受付中】"
              case _ =>
                s"${g.ownerName.value} x ${g.challengerName.getOrElse(ParticipantName.noName).value} 【対戦中】"
            },
            if (g.isPlaying) {
              if (g.isParticipating(p.participantId)) {
                <.button(
                  "再開する"
                )
              } else {
                <.button(
                  ^.onClick --> p.handler(LoadGame(g.gameId, p.participantId)),
                  "観戦する"
                )
              }
            } else {
              <.button(
                ^.onClick --> {
                  p.handler(EntryGame(g.gameId, p.participantId))
                },
                if (g.ownerId == p.participantId) ^.display.none else TagMod.empty,
                "参加する"
              )
            }
          )
        }.toTagMod,
        <.div(
          <.button(
            ^.onClick --> p.handler(CreateGame(p.participantId)),
            "新規ゲームを作成する"
          ),
          <.button(
            ^.onClick --> p.handler(BeginEditMode(p.participantId)),
            "【デバッグ】エディットモードを起動"
          ),
          reloadButton(p)
        )
      )
    def reloadButton(p: Props): VdomNode =
      <.button(
        ^.onClick --> p.handler(LoadGames(p.participantId)),
        "再読み込み"
      )
  }

  val Component = ScalaComponent.builder[Props]("EntranceComponent")
    .renderBackend[Backend]
    .build
}
