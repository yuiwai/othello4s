package othello.core

import othello.core
import utest._

object OthelloSpec extends TestSuite {
  val tests = Tests {
    test("new game") {
      Othello().board.value.collect { case (p, Black) => p } ==> Seq(Pos(5, 4), Pos(4, 5))
      Othello().board.value.collect { case (p, White) => p } ==> Seq(Pos(4, 4), Pos(5, 5))
    }
    test("chains") {
      Othello().chains(1, 1) ==> Set()
      Othello().chains(4, 4) ==> Set(
        Chain(Arrangement(4, 4, White), List(Arrangement(4, 5, Black))),
        Chain(Arrangement(4, 4, White), List(Arrangement(5, 4, Black))),
        Chain(Arrangement(4, 4, White), List(Arrangement(5, 5, White)))
      )
    }
    test("put") {
      Othello().put(4, 3, Black) match {
        case Right(o) =>
          o.gets((4, 3), (4, 4), (4, 5)) ==> Seq(Black, Black, Black)
          o.gets((5, 3), (5, 4), (5, 5)) ==> Seq(Black, White)
        case Left(e) => fail(s"put failed: $e")
      }
    }
    test("score") {
      Othello().score ==> Map(White -> 2, Black -> 2)
      Othello(Board(Map(Pos(1, 1) -> White)), Black).score ==> Map(Black -> 0, White -> 1)
    }
    test("can put all") {
      Othello().canPutAll.size ==> 4
      Othello(Board(Map(Pos(1, 1) -> White)), Black).canPutAll.size ==> 0
      Othello(Board(Map(Pos(1, 1) -> White, Pos(2, 1) -> Black)), Black).canPutAll.size ==> 0
      Othello(Board(Map(Pos(1, 1) -> White, Pos(2, 1) -> Black)), White).canPutAll.size ==> 1
    }
    test("put each other") {
      Othello().put(4, 3, Black).flatMap(_.put(3, 5, White)) match {
        case Right(o) =>
          o.gets((3, 5), (4, 5), (5, 5)) ==> Seq(White, White, White)
        case Left(e) => fail(s"put failed: $e")
      }
    }
    test("out of board") {
      Othello().put(9, 1, Black) ==> Left(OutOfBoard)
      Othello().put(1, 9, Black) ==> Left(OutOfBoard)
      Othello().put(1, 0, Black) ==> Left(OutOfBoard)
      Othello().put(0, 1, Black) ==> Left(OutOfBoard)
    }
    test("duplication") {
      Othello().put(4, 4, Black) ==> Left(StoneDuplication)
    }
    test("no reverse") {
      Othello().put(4, 6, Black) ==> Left(NoReverse)
    }
    test("other's turn") {
      Othello().put(4, 6, White) ==> Left(OthersTurn)
    }
    test("game over") {
      Othello().isGameOver ==> false
      core.Othello(Board((for {x <- 1 to 8; y <- 1 to 8; t = Pos(x, y) -> White} yield t).toMap), Black).isGameOver ==> true
      Othello(Board(Map(Pos(1, 1) -> White)), Black).isGameOver ==> true
    }
    test("greater color") {
      Othello().greaterColor ==> None
      Othello().put(4, 3, Black).right.get.greaterColor ==> Some(Black)
    }
  }
  def fail(msg: String): Unit = "" ==> msg
}

object ChainSpec extends TestSuite {
  val tests = Tests {
    test("reverse") {
      Chain(Arrangement(1, 1, Black), Nil)
        .reverse ==> Nil
      Chain(Arrangement(1, 1, Black), List(Arrangement(2, 1, White), Arrangement(3, 1, Black)))
        .reverse ==> List(Arrangement(2, 1, Black))
      Chain(Arrangement(1, 1, Black), List(Arrangement(2, 1, White), Arrangement(3, 1, White)))
        .reverse ==> Nil
      Chain(Arrangement(1, 1, Black), List(Arrangement(2, 1, White), Arrangement(3, 1, White), Arrangement(4, 1, Black)))
        .reverse ==> List(Arrangement(2, 1, Black), Arrangement(3, 1, Black))
    }
  }
}
