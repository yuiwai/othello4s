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
    val startedGame = notStartedGame.start.right.get
    test("entry") {
      test("valid") {
        val g = waitingGame.entry(challengerId).right.get
        g.state ==> Prepared
        g.version ==> waitingGame.version.increment
      }
      test("same as ownerId") {
        waitingGame.entry(ownerId) ==> Left(SameAsOwnerId)
      }
      test("not waiting") {
        notStartedGame.entry(challengerId) ==> Left(NotWaiting)
        startedGame.entry(challengerId) ==> Left(NotWaiting)
      }
    }
    test("cancel") {
      test("valid") {
        val g = waitingGame.cancel.get
        g.state ==> Canceled
        g.version ==> waitingGame.version.increment
      }
      test("not waiting") {
        startedGame.cancel ==> None
      }
    }
    test("start") {
      test("valid") {
        val g = notStartedGame.start.right.get
        g.state ==> Playing
        g.version ==> notStartedGame.version.increment
      }
      test("not prepared") {
        waitingGame.start ==> Left(NotPrepared)
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
        startedGame.putStone(ownerId, validPos).right.get.version ==> startedGame.version.increment
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
          Othello(Board(Map(Pos(1, 1) -> Black, Pos(1, 2) -> White)), Black),
          Playing, ownerId, Some(challengerId), true, GameVersion.first)
          .putStone(ownerId, Pos(1, 3)).right.get
        terminatedGame.greaterColor ==> Some(Black)
        terminatedGame.state ==> Terminated(Some(ownerId))
        terminatedGame.winner ==> Some(ownerId)
      }
      test("can accept version") {
        startedGame.canAcceptVersion(startedGame.version.increment) ==> true
        startedGame.canAcceptVersion(startedGame.version) ==> false
        startedGame.canAcceptVersion(startedGame.version.increment.increment) ==> false
        waitingGame.canAcceptVersion(notStartedGame.version) ==> false
      }
      test("timeout") {
        startedGame.timeout.state ==> Terminated(Some(challengerId))
      }
    }
  }
}

object GameVersionSpec extends TestSuite {
  val tests = Tests {
    test("accept") {
      GameVersion(1).accept(GameVersion(2)) ==> Some(GameVersion(2))
      GameVersion(1).accept(GameVersion(3)) ==> None
    }
  }
}
