package othello

import othello.core._
import othello.service._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ServiceImpl(gameRepository: GameRepository[Future], participantRepository: ParticipantRepository[Future])
  extends Service[Future] {
  override def participate(name: ParticipantName): Future[ParticipantId] =
    participantRepository.store(Participant(name))
  override def allGames(participantId: ParticipantId): Future[Seq[GameSummary]] =
    for {
      p <- participantRepository.find(participantId)
      g <- gameRepository.all.map(games => games.map {
        case (gameId, game) =>
          // FIXME 暫定でnoName
          GameSummary(gameId, game.state, game.ownerId, ParticipantName.noName, game.challengerId, Some(ParticipantName.noName))
      })
    } yield p.fold(Seq.empty[GameSummary])(_ => g)
  override def game(gameId: GameId): Future[Option[Game]] = gameRepository.find(gameId)
  override def createGame(participantId: ParticipantId): Future[Either[ServiceError, GameSummary]] =
    (for {
      p <- participantRepository.find(participantId)
      o <- gameRepository.ownedBy(participantId)
      if o.isEmpty // FIXME Serviceで判断を行なっている
      g <- gameRepository
        .store(Game(participantId))
        .map {
          // FIXME 暫定でnoName
          case Some(id) => Right(GameSummary(id, Waiting, participantId, ParticipantName.noName, None, None))
          case None => Left(GameStoreFailed)
        }
    } yield p.fold[Either[ServiceError, GameSummary]](Left(ParticipantNotFound))(_ => g)) recover {
      case _: NoSuchElementException => Left(OwnedGameExists)
    }
  override def entry(gameId: GameId, participantId: ParticipantId): Future[Either[ServiceError, EntryId]] =
    gameRepository
      .find(gameId)
      .flatMap {
        case Some(g) => g.entry(participantId) match {
          case Right(game) =>
            gameRepository
              .store(gameId, game.start)
              .map(_ => Right(EntryId(1000)))
          case Left(_) => Future.successful(Left(GameStarted)) // TODO エラーを細かく分類したい
        }
        case None => Future.successful(Left(GameNotFound))
      }
  override def putStone(
    gameId: GameId,
    participantId: ParticipantId,
    pos: Pos): Future[Either[ServiceError, Game]] =
    (for {
      p <- participantRepository.find(participantId)
      g <- gameRepository.find(gameId)
    } yield {
      for {
        _ <- p.fold[Either[ServiceError, _]](Left(ParticipantNotFound))(x => Right(x))
        game <- g.fold[Either[ServiceError, Game]](Left(GameNotFound))(x => Right(x))
      } yield game.putStone(participantId, pos)
    }).flatMap {
      case Right(x) => x match {
        case Right(g) =>
          gameRepository.store(gameId, g)
            .map(_.fold[Either[ServiceError, Game]](Left(GameStoreFailed))(game => Right(game)))
        case Left(_) => Future.successful(Left(PutError))
      }
      case Left(e) => Future.successful(Left(e))
    }
  override def pass(gameId: GameId, participantId: ParticipantId): Future[Either[ServiceError, Game]] =
    (for {
      p <- participantRepository.find(participantId)
      g <- gameRepository.find(gameId)
    } yield {
      for {
        _ <- p.fold[Either[ServiceError, _]](Left(ParticipantNotFound))(x => Right(x))
        game <- g.fold[Either[ServiceError, Game]](Left(GameNotFound))(x => Right(x))
      } yield game.pass(participantId)
    }).flatMap {
      case Right(x) => x match {
        case Right(passedGame) => gameRepository.store(gameId, passedGame)
          .map(_.fold[Either[ServiceError, Game]](Left(GameStoreFailed))(Right(_)))
        case Left(_) => Future.successful(Left(PassError))
      }
      case Left(e) => Future.successful(Left(e))
    }

  override def giveUp(gameId: GameId, participantId: ParticipantId): Future[Either[ServiceError, Game]] =
    (for {
      p <- participantRepository.find(participantId)
      g <- gameRepository.find(gameId)
    } yield {
      for {
        _ <- p.fold[Either[ServiceError, _]](Left(ParticipantNotFound))(x => Right(x))
        game <- g.fold[Either[ServiceError, Game]](Left(GameNotFound))(x => Right(x))
      } yield game.giveUp
    }).flatMap {
      case Right(game) =>
        gameRepository.store(gameId, game).map(_ => Right(game))
      case other => Future.successful(other)
    }
}

class InMemoryGameRepository extends GameRepository[Future] {
  private val games: mutable.Map[GameId, Game] = mutable.Map.empty
  override def all: Future[Seq[(GameId, Game)]] = Future.successful(games.toSeq)
  override def find(gameId: GameId): Future[Option[Game]] = Future.successful(games.get(gameId))
  override def ownedBy(ownerId: ParticipantId): Future[Option[Game]] =
    Future.successful(games.values.find(_.ownerId == ownerId))
  override def store(gameId: GameId, game: Game): Future[Option[Game]] = {
    games.update(gameId, game)
    Future.successful(Some(game))
  }
  override def store(game: Game): Future[Option[GameId]] = {
    val gameId = GameId(games.size + 1)
    store(gameId, game).map(_.map(_ => gameId))
  }
  override def delete(gameId: GameId): Future[Option[GameId]] = {
    if (games.exists(_._1 == gameId)) {
      games -= gameId
      Future.successful(Some(gameId))
    }
    else Future.successful(None)
  }
}

class InMemoryParticipantRepository extends ParticipantRepository[Future] {
  private val participants: mutable.Map[ParticipantId, Participant] = mutable.Map.empty
  override def find(participantId: ParticipantId): Future[Option[Participant]] =
    Future.successful(participants.get(participantId))
  override def findByIds(participantIds: Seq[ParticipantId]): Future[Map[ParticipantId, Participant]] = {
    Future.traverse(participantIds)(participantId => find(participantId).map(participantId -> _))
      .map(_.foldLeft(Map.empty[ParticipantId, Participant]) {
        case (acc, (participantId, Some(participant))) => acc.updated(participantId, participant)
        case (acc, _) => acc
      })
  }
  override def store(participant: Participant): Future[ParticipantId] = {
    val participantId = ParticipantId(participants.size + 1)
    participants.update(participantId, participant)
    Future.successful(participantId)
  }
}