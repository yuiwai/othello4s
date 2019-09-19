package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import othello.core.ParticipantId
import othello.service.GameSummary

object EntranceComponent {

  final case class Props(participantId: ParticipantId, games: Seq[GameSummary], handler: Action => Callback) {
    @inline def render: VdomElement = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomElement =
      <.div(
        p.games.map { g =>
          <.div(
            g.gameId.value,
            s"(${g.gameState})",
            if (g.isPlaying) {
              if (g.isParticipating(p.participantId)) {
                <.button(
                  "Open"
                )
              } else {
                <.button(
                  "Watch"
                )
              }
            } else {
              <.button(
                ^.onClick --> {
                  p.handler(EntryGame(g.gameId, p.participantId))
                },
                if (g.ownerId == p.participantId) ^.display.none else TagMod.empty,
                "Entry"
              )
            }
          )
        }.toTagMod,
        <.button(
          ^.onClick --> p.handler(CreateGame(p.participantId)),
          "create game"
        )
      )
  }

  val Component = ScalaComponent.builder[Props]("EntranceComponent")
    .renderBackend[Backend]
    .build
}
