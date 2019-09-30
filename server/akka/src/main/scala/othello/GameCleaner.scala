package othello

import akka.actor.{Actor, Props}
import othello.GameCleaner.ThinkingTime
import othello.GameEventActor.UnSubscribeAll
import othello.core.{Game, GameVersion}
import othello.service.{GameId, GameRepository, Terminated}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

class GameCleaner(gameRepository: GameRepository[Future], timeoutCount: Int = 60) extends Actor {
  import context.dispatcher
  val thinkingTimeMap = mutable.Map.empty[GameId, ThinkingTime]
  override def preStart(): Unit = {
    context.system.scheduler.schedule(1.second, 1.seconds, self, GameCleaner.Tick)
  }

  def receive: Receive = {
    case GameCleaner.Tick =>
      gameRepository
        .all
        .foreach(_.filter(_._2.isTerminated).foreach {
          case (gameId, _) =>
            context.system.actorSelection("user/" + GameEventActor.actorName(gameId)) ! UnSubscribeAll
            gameRepository.delete(gameId)
        })
      gameRepository
        .all
        .foreach(_.filter(_._2.isCanceled).foreach {
          case (gameId, _) =>
            context.system.actorSelection("user/" + GameEventActor.actorName(gameId)) ! UnSubscribeAll
            gameRepository.delete(gameId)
        })
      gameRepository
        .all
        .foreach(_.filter(_._2.isPlaying).foreach {
          case (gameId, game) =>
            thinkingTimeMap.get(gameId) match {
              case Some(t) =>
                t.timeoutOrIncrement(game.version) match {
                  case Some(tt) => thinkingTimeMap.update(gameId, tt)
                  case None =>
                    val terminatedGame = game.timeout
                    gameRepository.store(gameId, terminatedGame)
                    context.system.actorSelection("user/" + GameEventActor.actorName(gameId)) !
                      Terminated(gameId, terminatedGame.version)
                    thinkingTimeMap.remove(gameId)
                }
              case None =>
                ThinkingTime(game, timeoutCount).foreach(thinkingTimeMap.update(gameId, _))
            }
        })
  }
}

object GameCleaner {
  object Tick
  case class ThinkingTime(version: GameVersion, limit: Int, current: Int) {
    def timeoutOrIncrement(checkVersion: GameVersion): Option[ThinkingTime] = current + 1 match {
      case c if version == checkVersion && c >= limit => None
      case c if version == checkVersion => Some(copy(current = c))
      case _ => Some(copy(checkVersion, current = 0))
    }
  }
  object ThinkingTime {
    def apply(game: Game, limit: Int, current: Int = 0): Option[ThinkingTime] =
      game.currentTurn.map(_ => apply(game.version, limit, current))
  }
  def props(gameRepository: GameRepository[Future]): Props = Props(new GameCleaner(gameRepository))
}
