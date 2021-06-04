import $ivy.`dev.zio::zio:1.0.3`
import $ivy.`dev.zio::zio-streams:1.0.3`

import zio._

case class Http[-R, +E, -A, +B](run: A => ZIO[R, E, Option[B]]) { self =>
  def +++[R1 <: R, E1 >: E, A1 <: A, B1 >: B](other: Http[R1, E1, A1, B1]): Http[R1, E1, A1, B1] =
    Http(a =>
      self.run(a) >>= {
        case Some(z) => ZIO.succeed(Option(z))
        case None    => other.run(a)
      },
    )

  def flatMap[R1 <: R, E1 >: E, A1 <: A, B1 >: B, C](f: B1 => Http[R1, E1, A1, C]): Http[R1, E1, A1, C] =
    Http((a: A1) =>
      self.run(a) >>= {
        case Some(b) => f(b).run(a)
        case None    => ZIO.none
      },
    )
}
