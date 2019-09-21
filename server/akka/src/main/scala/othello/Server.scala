package othello

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.scaladsl.Source
import akka.stream.{ActorMaterializer, OverflowStrategy}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.generic.auto._
import io.circe.syntax._
import othello.GameEventActor.Subscribe
import othello.core.ParticipantId
import othello.service._

import scala.collection.mutable
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._

object Server extends FailFastCirceSupport with codec.Codec {
  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  val service = new ServiceImpl(new InMemoryGameRepository, new InMemoryParticipantRepository)
  val gameEventActors = mutable.Map.empty[GameId, ActorRef]

  def main(args: Array[String]) {
    import akka.http.scaladsl.marshalling.sse.EventStreamMarshalling._
    val route: Route =
      concat(
        path("participants" / "create") {
          post {
            complete(service.participate)
          }
        },
        path("games") {
          post {
            entity(as[ParticipantId]) { participantId =>
              complete(service.allGames(participantId))
            }
          }
        },
        path("games" / IntNumber) { id =>
          get {
            onSuccess(service.game(GameId(id))) {
              _.fold(reject)(g => complete(g))
            }
          }
        },
        path("games" / "create") {
          post {
            entity(as[ParticipantId]) { participantId =>
              complete {
                service.createGame(participantId)
                  .map(_.map { gs =>
                    createGameEventActor(gs.gameId)
                    gs
                  })
              }
            }
          }
        },
        path("games" / "entry") {
          post {
            entity(as[EntryRequest]) { case EntryRequest(gameId, participantId) =>
              complete(service.entry(gameId, participantId))
            }
          }
        },
        path("games" / "putStone") {
          post {
            entity(as[PutStoneRequest]) { case PutStoneRequest(gameId, participantId, pos) =>
              complete(service.putStone(gameId, participantId, pos).map {
                case r@Right(game) =>
                  sendGameEvent(gameId, StonePut(participantId, pos, game.version))
                  r
                case l@Left(_) => l
              })
            }
          }
        },
        path("games" / "giveUp") {
          post {
            entity(as[GiveUpRequest]) { case GiveUpRequest(gameId, participantId) =>
              complete(service.giveUp(gameId, participantId).map {
                case r@Right(game) =>
                  sendGameEvent(gameId, GivenUp(game.version))
                  r
                case l@Left(_) => l
              })
            }
          }
        },
        path("events" / IntNumber) { id =>
          get {
            complete {
              Source
                .actorRef[GameEvent](10, OverflowStrategy.dropBuffer)
                .map(gameEvent => ServerSentEvent(gameEvent.asJson.noSpaces))
                .keepAlive(1.second, () => ServerSentEvent.heartbeat)
                .mapMaterializedValue { actorRef =>
                  subscribeGameEvent(actorRef, GameId(id))
                }
            }
          }
        },
        pathPrefix("assets") {
          getFromResourceDirectory("assets/")
        },
        pathPrefix("public") {
          getFromResourceDirectory("public/")
        }
      )
    Http().bindAndHandle(route, "0.0.0.0", 8080)
  }

  def subscribeGameEvent(subscriber: ActorRef, gameId: GameId): Unit =
    gameEventActors.get(gameId).foreach(_ ! Subscribe(subscriber, gameId))

  def createGameEventActor(gameId: GameId): Unit = {
    if (!gameEventActors.exists(_._1 == gameId)) {
      gameEventActors(gameId) = system.actorOf(GameEventActor.props(gameId), GameEventActor.actorName(gameId))
    }
  }

  // FIXME 暫定で固定のイベントを送信
  def sendGameEvent(gameId: GameId, event: GameEvent): Unit = {
    gameEventActors.get(gameId) foreach (_ ! event)
  }
}

class GameEventActor(myGameId: GameId) extends Actor {
  override def receive: Receive = behavior(Nil)
  private def behavior(subscribers: List[ActorRef]): Receive = {
    case Subscribe(subscriber: ActorRef, gameId: GameId) =>
      if (myGameId == gameId) context.become(behavior(subscriber :: subscribers))
    case e: GameEvent =>
      subscribers foreach (_ ! e)
    case _ =>
  }
}

object GameEventActor {
  val prefix = "game-event-actor"
  def actorName(gameId: GameId): String = s"$prefix-${gameId.value}"
  def props(gameId: GameId): Props = Props(new GameEventActor(gameId))
  final case class Subscribe(subscriber: ActorRef, gameId: GameId)
}
