import $ivy.`dev.zio::zio:1.0.3`
import $ivy.`dev.zio::zio-streams:1.0.3`

import zio._

sealed trait HttpResult[-R, +E, +A] {
  def asZIO: ZIO[R, E, A]
  def flatMap[R1 <: R, E1 >: E, B](f: A => HttpResult[R1, E1, B]): HttpResult[R1, E1, B]
}

object HttpResult {
  def succeed[A](a: A): HttpResult[Any, Nothing, A] = ???
}

sealed trait Http[-R, +E, -A, +B] { self =>
  def apply(a: A): HttpResult[R, E, Option[B]] = Http.run(a, self)

  def +++[R1 <: R, E1 >: E, A1 <: A, B1 >: B](other: Http[R1, E1, A1, B1]): Http[R1, E1, A1, B1] =
    Http.Combine(self, other)

  def flatMap[R1 <: R, E1 >: E, A1 <: A, B1 >: B, C](f: B1 => Http[R1, E1, A1, C]): Http[R1, E1, A1, C] =
    Http.FlatMap(self, f)
}

object Http {
  case class Combine[R, E, A, B](h1: Http[R, E, A, B], h2: Http[R, E, A, B])         extends Http[R, E, A, B]
  case class FlatMap[R, E, A, B, C](h1: Http[R, E, A, B], h2: B => Http[R, E, A, C]) extends Http[R, E, A, C]

  def run[R, E, A, B](a: A, http: Http[R, E, A, B]): HttpResult[R, E, Option[B]] =
    http match {
      case Combine(h1, h2) =>
        h1(a) flatMap {
          case Some(b) => HttpResult.succeed(Option(b))
          case None    => h2(a)
        }
      case FlatMap(h1, f)  =>
        h1(a) flatMap { b =>
          f(b)(a)
        }
    }
}
