package othello

import othello.core._
import othello.service._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ServiceImpl(gameRepository: GameRepository[Future], participantRepository: ParticipantRepository[Future])
  extends Service[Future] {
  override def participate: Future[ParticipantId] = participantRepository.create
  override def allGames(participantId: ParticipantId): Future[Seq[GameSummary]] =
    for {
      p <- participantRepository.find(participantId)
      g <- gameRepository.all.map(_.map {
        case (gameId, game) => GameSummary(gameId, game.state, game.ownerId, game.challengerId)
      })
    } yield p.fold(Seq.empty[GameSummary])(_ => g)
  override def game(gameId: GameId): Future[Option[Game]] = gameRepository.find(gameId)
  override def createGame(participantId: ParticipantId): Future[Either[ServiceError, GameSummary]] =
    (for {
      p <- participantRepository.find(participantId)
      o <- gameRepository.ownedBy(participantId)
      if o.isEmpty // FIXME Serviceで判断を行なっている
      g <- gameRepository
        .create(participantId)
        .map(id => Right(GameSummary(id, Waiting, participantId, None)))
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
      case Right(g) => g match {
        case Right(g) =>
          gameRepository.store(gameId, g)
            .map(_.fold[Either[ServiceError, Game]](Left(GameStoreFailed))(game => Right(game)))
        case Left(_) => Future.successful(Left(PutError))
      }
      case Left(e) => Future.successful(Left(e))
    }
}

class InMemoryGameRepository extends GameRepository[Future] {
  private val games: mutable.Map[GameId, Game] = mutable.Map.empty
  override def all: Future[Seq[(GameId, Game)]] = Future.successful(games.toSeq)
  override def find(gameId: GameId): Future[Option[Game]] = Future.successful(games.get(gameId))
  override def create(ownerId: ParticipantId): Future[GameId] = {
    val gameId = GameId(games.size + 1)
    games.update(gameId, Game(ownerId))
    Future.successful(gameId)
  }
  override def ownedBy(ownerId: ParticipantId): Future[Option[Game]] =
    Future.successful(games.values.find(_.ownerId == ownerId))
  override def store(gameId: GameId, game: Game): Future[Option[Game]] = {
    games.update(gameId, game)
    Future.successful(Some(game))
  }
}

class InMemoryParticipantRepository extends ParticipantRepository[Future] {
  private val participants: mutable.Map[ParticipantId, Participant] = mutable.Map.empty
  override def find(participantId: ParticipantId): Future[Option[Participant]] =
    Future.successful(participants.get(participantId))
  override def create: Future[ParticipantId] = {
    val participantId = ParticipantId(participants.size + 1)
    participants.update(participantId, Free() /* FIXME Participant impl */)
    Future.successful(participantId)
  }
}