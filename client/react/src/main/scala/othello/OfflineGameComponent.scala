package othello

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import othello.core.{Arrangement, Game, Pos}
import othello.service.GameDetail

import scala.concurrent.duration._

object OfflineGameComponent {

  sealed trait OfflineGameMode {
    val boardClickHandle: Pos => Callback
  }
  case object Preparing extends OfflineGameMode {
    override val boardClickHandle: Pos => Callback = _ => Callback.empty
  }
  case class SinglePlayMode(bs: BackendScope[Props, State]) extends OfflineGameMode {
    override val boardClickHandle: Pos => Callback = { pos =>
      bs.modState { s =>
        s.game.currentTurn match {
          case Some(participantId) =>
            s.game.putStone(participantId, pos) match {
              case Right(g) => s.putGame(g)
              case Left(_) => s
            }
          case None => s
        }
      }
    }
  }
  case class AIPlayMode(bs: BackendScope[Props, State]) extends OfflineGameMode {
    override val boardClickHandle: Pos => Callback = { pos =>
      (bs.modState { s =>
        import s.game
        if (game.nextIsOwner) {
          game.putStone(game.ownerId, pos) match {
            case Right(g) => s.putGame(g)
            case Left(_) => s
          }
        } else s
      }.async >> bs.modState(s => s.putGame(selectNextArrangement(s.game))).delay(500.millis)).toCallback
    }
    def selectNextArrangement(game: Game): Game =
      if (!game.nextIsOwner) {
        (game.challengerId match {
          case Some(challengerId) =>
            game.othello.canPutAll match {
              case xs: Set[Arrangement] if xs.nonEmpty =>
                xs.tail.foldLeft(xs.head -> Double.MinValue) {
                  case (acc@(current, score), arrangement) =>
                    calcArrangementScore(game, arrangement) match {
                      case Some(newScore) if newScore > score => arrangement -> newScore
                      case _ => acc
                    }
                } match {
                  case (a, _) => game.putStone(challengerId, a.pos)
                }
              case _ => game.pass(challengerId)
            }
          case None => Right(game)
        }) match {
          case Right(g) => g
          case Left(_) => game
        }
      } else game
    def calcArrangementScore(game: Game, arrangement: Arrangement): Option[Double] =
      game.othello
        .put(arrangement.pos, arrangement.color)
        .map(_.score(arrangement.color).toDouble + (arrangement.pos match {
          case Pos(1, 1) | Pos(8, 8) | Pos(1, 8) | Pos(8, 1) => 20
          case Pos(1, _) | Pos(_, 1) | Pos(8, _) | Pos(_, 8) => 10
          case _ => 0
        }))
        .toOption
  }

  final case class Props(handler: Action => Callback) {
    @inline def render: VdomElement = Component(this)
  }
  final case class State(gameDetail: GameDetail, mode: OfflineGameMode) {
    def game: Game = gameDetail.game
    def putGame(game: Game): State = copy(gameDetail = gameDetail.copy(game = game))
  }

  final class Backend($: BackendScope[Props, State]) {
    def render(p: Props, s: State): VdomElement =
      <.div(
        s.mode match {
          case Preparing =>
            <.div(
              <.button(
                ^.onClick --> $.modState(_.copy(mode = SinglePlayMode($))),
                "1人プレイを開始"
              ),
              <.button(
                ^.onClick --> $.modState(_.copy(mode = AIPlayMode($))),
                "AIと対戦を開始"
              )
            )
          case _: SinglePlayMode =>
            <.div(
              "1人プレイ中..."
            )
          case _: AIPlayMode =>
            <.div(
              "AIと対戦中...",
              GameInformationBar.render(
                s.gameDetail,
                s.game.ownerId,
                () => Callback.empty,
                () => $.setState(s.putGame(s.game.pass(s.game.ownerId).fold(_ => s.game, identity))),
                () => $.setState(s.putGame(s.game.giveUp)),
                () => Callback.empty,
                <.button(
                  ^.onClick --> $.setState(s.putGame(Game.singlePlay)),
                  "再戦する"
                )
              )
            )
        },
        BoardComponent.Props(s.game.othello.board, s.mode.boardClickHandle).render,
        ScoreView.Props(s.game.othello.score).render,
        <.div(
          <.button(
            ^.onClick --> p.handler(Initialize),
            "トップに戻る"
          )
        )
      )
  }

  val Component = ScalaComponent.builder[Props]("OfflineGameComponent")
    .initialState(State(GameDetail.singlePlay, Preparing))
    .renderBackend[Backend]
    .build
}
