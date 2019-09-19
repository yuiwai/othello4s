package othello.check

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import othello.codec.Codec
import othello.core._
import othello.service.{ServiceError, StonePut}

object Check extends Codec {
  def main(args: Array[String]): Unit = {
    val game = Game(ParticipantId(10))
    println(decode[Either[ServiceError, Game]](game.asJson.toString))

    println(decode[StonePut](StonePut(ParticipantId(1), Pos(1, 2), 1).asJson.noSpaces))
  }
}