package othello.core

import utest._

object GameSpec extends TestSuite {
  val tests = Tests {
    val ownerId = ParticipantId(1)
    val challengerId = ParticipantId(2)
    val validPos = Pos(4, 3)
    val invalidPos = Pos(1, 1)
    val waitingGame = Game(ownerId)
    val notStartedGame = waitingGame.entry(challengerId).right.get
    val startedGame = notStartedGame.start
    test("entry") {
      test("valid") {
        waitingGame.entry(challengerId).right.get.state ==> Prepared
      }
      test("same as ownerId") {
        waitingGame.entry(ownerId) ==> Left(SameAsOwnerId)
      }
      // FIXME なぜエントリできないのかを表現できた方がいい
      test("not waiting") {
        notStartedGame.entry(challengerId) ==> Left(NotWaiting)
        startedGame.entry(challengerId) ==> Left(NotWaiting)
      }
    }
    test("start") {
      test("valid") {
        // TODO
      }
      test("not prepared") {
        // TODO
      }
    }
    test("participantIdFromColor") {
      startedGame.participantIdFromColor(Black) ==> Some(ownerId)
      startedGame.participantIdFromColor(White) ==> Some(challengerId)
    }
    test("giveUp") {
      test("valid") {
        startedGame.giveUp.state ==> Terminated(Some(challengerId))
      }
    }
    test("put stone") {
      test("valid") {
        startedGame.putStone(ownerId, validPos).right.get.version ==> 2
      }
      test("waiting") {
        waitingGame.putStone(ownerId, validPos) ==> Left(NotPlaying)
      }
      test("not started") {
        notStartedGame.putStone(ownerId, validPos) ==> Left(NotPlaying)
      }
      test("invalid pos") {
        startedGame.putStone(ownerId, invalidPos) ==> Left(InvalidPos(NoReverse))
      }
      test("not playing") {
        startedGame.giveUp.putStone(ownerId, validPos) ==> Left(NotPlaying)
      }
      test("is not turn") {
        startedGame.putStone(challengerId, validPos) ==> Left(IsNotTurn)
      }
      test("terminated") {
        val terminatedGame = Game(
          Othello(Map(Pos(1, 1) -> Black, Pos(1, 2) -> White), Black),
          Playing, ownerId, Some(challengerId), true, 1)
          .putStone(ownerId, Pos(1, 3)).right.get
        terminatedGame.greaterColor ==> Some(Black)
        terminatedGame.state ==> Terminated(Some(ownerId))
        terminatedGame.winner ==> Some(ownerId)
      }
    }
  }
}