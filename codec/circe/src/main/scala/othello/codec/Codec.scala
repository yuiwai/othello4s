package othello.codec

import io.circe._
import othello.core._
import othello.service._

trait Codec {
  implicit val posKeyEncoder: KeyEncoder[Pos] = pos => s"${pos.x}-${pos.y}"
  implicit val encodeStoneColor: Encoder[StoneColor] = color => Json.fromString(color.toString)
  implicit val encodeServiceError: Encoder[ServiceError] = e => Json.fromString(e.toString)
  implicit val encodeGameState: Encoder[GameState] = gs => Json.fromString(gs.toString)

  implicit val posKeyDecoder: KeyDecoder[Pos] = _.split('-') match {
    case Array(x, y) => Some(Pos(x.toInt, y.toInt))
    case _ => None
  }
  implicit val decodeStoneColor: Decoder[StoneColor] = Decoder.decodeString.emap {
    case "White" => Right(White)
    case "Black" => Right(Black)
    case other => Left(s"Unknown stone color: $other")
  }
  implicit val decodeServiceError: Decoder[ServiceError] = Decoder.decodeString.emap {
    e => ServiceError(e).fold[Either[String, ServiceError]](Left(s"Unknown service error :$e"))(Right(_))
  }
  implicit val decodeGameState: Decoder[GameState] = Decoder.decodeString.emap {
    case "Waiting" => Right(Waiting)
    case "Prepared" => Right(Prepared)
    case "Canceled" => Right(Canceled)
    case "Playing" => Right(Playing)
    case "Terminated" => Right(Terminated)
    case other => Left(s"Unknown game state: $other")
  }
  implicit def h[A,B](implicit a: Decoder[A], b: Decoder[B]): Decoder[Either[A,B]] = {
    val l: Decoder[Either[A,B]]= a.map(Left.apply)
    val r: Decoder[Either[A,B]]= b.map(Right.apply)
    l or r
  }
}