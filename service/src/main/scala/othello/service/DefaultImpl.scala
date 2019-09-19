package othello.service

import scala.language.higherKinds

trait DefaultImpl[F[_]] { self: Service[F] =>
//  protected val repository: GameRepository
}

