package othello.core

final case class Othello(stones: Map[Pos, StoneColor], turn: StoneColor) {
  def get(pos: Pos): Option[StoneColor] = stones.get(pos)
  def get(x: Int, y: Int): Option[StoneColor] = get(Pos(x, y))
  def gets(xs: => Seq[Pos]): Seq[StoneColor] = xs.map(get).collect { case Some(x) => x }
  def gets(xs: (Int, Int)*): Seq[StoneColor] = gets(xs.map(Pos.tupled))
  def greaterColor: Option[StoneColor] = score(Black) -> score(White) match {
    case (b, w) if b > w => Some(Black)
    case (b, w) if b < w => Some(White)
    case _ => None
  }
  def chain(origin: Pos, direction: Direction): Option[Chain] = {
    get(origin).flatMap { c =>
      @scala.annotation.tailrec
      def impl(current: Pos, xs: List[Arrangement]): Option[Chain] = {
        get(current) match {
          case Some(cc) => impl(current + direction, Arrangement(current, cc) :: xs)
          case None => if (xs.isEmpty) None else Some(Chain(Arrangement(origin, c), xs.reverse))
        }
      }

      impl(origin + direction, Nil)
    }
  }
  def chains(origin: Pos): Set[Chain] = Directions.all.map(chain(origin, _)).collect { case Some(c) => c }
  def chains(x: Int, y: Int): Set[Chain] = chains(Pos(x, y))
  def score: Map[StoneColor, Int] = stones.foldLeft[Map[StoneColor, Int]](Map(White -> 0, Black -> 0)) {
    case (acc, (_, color)) => acc.updated(color, acc(color) + 1)
  }
  def canPutAll: Set[Arrangement] =
    (for {
      x <- 1 to 8
      y <- 1 to 8
      if put(x, y, turn).isRight
    } yield Arrangement(x, y, turn)).toSet
  def isGameOver: Boolean = score.values.sum == 64 || (canPutAll.isEmpty && pass.canPutAll.isEmpty)
  private def mapStone(pos: Pos)(f: Option[StoneColor] => StoneColor): Othello =
    copy(stones = stones.updated(pos, f(stones.get(pos))))
  def put(pos: Pos, color: StoneColor): Either[OthelloError, Othello] = {
    if (pos.x < 1 || pos.x > 8 || pos.y < 1 || pos.y > 8) Left(OutOfBoard)
    else if (stones.exists(_._1 == pos)) Left(StoneDuplication)
    else if (color != turn) Left(OthersTurn)
    else mapStone(pos) { _ => color }
      .applyReverse(pos)
      .map(_.pass)
  }
  def put(x: Int, y: Int, color: StoneColor): Either[OthelloError, Othello] = put(Pos(x, y), color)
  def applyReverse(pos: Pos): Either[OthelloError, Othello] =
    chains(pos).flatMap(_.reverse) match {
      case xs if xs.isEmpty => Left(NoReverse)
      case xs => Right(xs.foldLeft(this) { case (acc, a) => acc.mapStone(a.pos)(_ => a.color) })
    }
  def pass: Othello = copy(turn = turn.reverse)
}

object Othello {
  def apply(): Othello = apply(Map(
    Pos(4, 4) -> White,
    Pos(5, 4) -> Black,
    Pos(4, 5) -> Black,
    Pos(5, 5) -> White
  ), Black)
}

final case class Pos(x: Int, y: Int) {
  def +(direction: Direction): Pos = Pos(x + direction.x, y + direction.y)
}
final case class Arrangement(pos: Pos, color: StoneColor) {
  def reverse: Arrangement = copy(color = color.reverse)
}
object Arrangement {
  def apply(x: Int, y: Int, color: StoneColor): Arrangement = apply(Pos(x, y), color)
}
final case class Chain(origin: Arrangement, stones: List[Arrangement]) {
  @scala.annotation.tailrec
  private def reverseImpl(xs: List[Arrangement], ys: List[Arrangement]): List[Arrangement] =
    ys match {
      case Nil => Nil
      case h :: _ if h.color == origin.color => xs.reverse
      case h :: t => reverseImpl(h.reverse :: xs, t)
    }
  def reverse: List[Arrangement] = stones match {
    case Nil => Nil
    case h :: _ if h.color == origin.color => Nil
    case xs => reverseImpl(Nil, xs)
  }
}

sealed abstract case class Direction(x: Int, y: Int)
object Directions {
  val all: Set[Direction] = Set(Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight)
  object Top extends Direction(0, -1)
  object Bottom extends Direction(0, 1)
  object Left extends Direction(-1, 0)
  object Right extends Direction(1, 0)
  object TopLeft extends Direction(-1, -1)
  object TopRight extends Direction(1, -1)
  object BottomLeft extends Direction(-1, 1)
  object BottomRight extends Direction(1, 1)
}

sealed trait StoneColor {
  def reverse: StoneColor
}
case object Black extends StoneColor {
  override def reverse: StoneColor = White
}
case object White extends StoneColor {
  override def reverse: StoneColor = Black
}

sealed trait OthelloError
case object OutOfBoard extends OthelloError
case object StoneDuplication extends OthelloError
case object NoReverse extends OthelloError
case object OthersTurn extends OthelloError
