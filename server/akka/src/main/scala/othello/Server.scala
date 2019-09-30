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
import othello.GameCleaner.ThinkingTime
import othello.GameEventActor.{Ping, Pong, Subscribe, UnSubscribeAll}
import othello.core.{Game, GameVersion, ParticipantId}
import othello.service._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}

object Server extends FailFastCirceSupport with codec.Codec {
  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  val gameRepository = new InMemoryGameRepository
  val participantRepository = new InMemoryParticipantRepository
  val service = new ServiceImpl(gameRepository, participantRepository)
  val gameEventActors = mutable.Map.empty[GameId, ActorRef]
  val gameCleaner = system.actorOf(GameCleaner.props(gameRepository), "game-cleaner")

  def main(args: Array[String]) {
    import akka.http.scaladsl.marshalling.sse.EventStreamMarshalling._
    val route: Route =
      concat(
        path("participants" / "create") {
          post {
            entity(as[ParticipateRequest]) { request =>
              complete(service.participate(request.name))
            }
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
        path("games" / "cancel") {
          post {
            entity(as[CancelGameRequest]) { case CancelGameRequest(gameId, participantId) =>
              complete {
                service.cancel(gameId, participantId)
              }
            }
          }
        },
        path("games" / "entry") {
          post {
            entity(as[EntryRequest]) { case EntryRequest(gameId, participantId) =>
              sendGameEvent(gameId, GameStarted(gameId, participantId))
              complete(service.entry(gameId, participantId))
            }
          }
        },
        path("games" / "putStone") {
          post {
            entity(as[PutStoneRequest]) { case PutStoneRequest(gameId, participantId, pos) =>
              complete(service.putStone(gameId, participantId, pos).map {
                case r@Right(game) =>
                  sendGameEvent(gameId, StonePut(gameId, participantId, pos, game.version))
                  if (game.isTerminated) sendGameEvent(gameId, Terminated(gameId, game.version))
                  r
                case l@Left(_) => l
              })
            }
          }
        },
        path("games" / "pass") {
          post {
            entity(as[PassRequest]) { case PassRequest(gameId, participantId) =>
              complete(service.pass(gameId, participantId))
            }
          }
        },
        path("games" / "giveUp") {
          post {
            entity(as[GiveUpRequest]) { case GiveUpRequest(gameId, participantId) =>
              complete(service.giveUp(gameId, participantId).map {
                case r@Right(game) =>
                  sendGameEvent(gameId, GivenUp(gameId, game.version))
                  sendGameEvent(gameId, Terminated(gameId, game.version))
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
        pathPrefix("public") {
          getFromResourceDirectory("public/")
        },
        pathPrefix("assets") {
          getFromResourceDirectory("assets/")
        },

        // DEBUG
        path("games" / "createCustom") {
          post {
            entity(as[CreateCustomGameRequest]) { case CreateCustomGameRequest(participantId, board) =>
              complete {
                service.createCustomGame(participantId, board)
                  .map {
                    case r@Some(gameId) =>
                      createGameEventActor(gameId)
                      r
                    case _ => None
                  }
              }
            }
          }
        }
      )
    Http().bindAndHandle(route, "0.0.0.0", 8080)
  }

  def subscribeGameEvent(subscriber: ActorRef, gameId: GameId): Unit =
    gameEventActors.get(gameId).foreach(_ ! Subscribe(subscriber, gameId))

  def unSubscribeGameEventAll(gameId: GameId): Unit =
    gameEventActors.get(gameId).foreach(_ ! UnSubscribeAll(gameId))

  def createGameEventActor(gameId: GameId): Unit = {
    if (!gameEventActors.exists(_._1 == gameId)) {
      gameEventActors(gameId) = system.actorOf(GameEventActor.props(gameId), GameEventActor.actorName(gameId))
    }
  }

  def sendGameEvent(gameId: GameId, event: GameEvent): Unit = {
    gameEventActors.get(gameId) foreach (_ ! event)
  }
}

class GameEventActor(myGameId: GameId) extends Actor {
  override def receive: Receive = behavior(Nil)
  private def behavior(subscribers: List[ActorRef]): Receive = {
    case Subscribe(subscriber: ActorRef, gameId: GameId) =>
      if (myGameId == gameId) context.become(behavior(subscriber :: subscribers))
    case UnSubscribeAll(gameId) =>
      if (myGameId == gameId) context.become(behavior(Nil))
    case e: GameEvent =>
      subscribers foreach (_ ! e)
    case Ping => sender() ! Pong
    case _ =>
  }
}

object GameEventActor {
  val prefix = "game-event-actor"
  def actorName(gameId: GameId): String = s"$prefix-${gameId.value}"
  def props(gameId: GameId): Props = Props(new GameEventActor(gameId))
  final case class Subscribe(subscriber: ActorRef, gameId: GameId)
  final case class UnSubscribeAll(gameId: GameId)
  case object Ping
  case object Pong
}

