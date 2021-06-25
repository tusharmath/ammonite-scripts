import $ivy.`dev.zio::zio:1.0.3`
import $ivy.`dev.zio::zio-streams:1.0.3`

import zio._

object Http_2 {
  sealed trait Event[+A]
  object Event {
    case class Read[A](data: A) extends Event[A]
    case object Complete        extends Event[Nothing]
  }

  type Request

  type Http[R, E, A, B]
  object Http {
    def collectHttp[A]: MkCollect[A] = ???

    final case class MkCollect[A]() {
      def apply[R, E, B](pf: PartialFunction[A, Http[R, E, A, B]]): Http[R, E, A, B] = ???
    }
  }

  Http.collectHttp[Request] { case req =>
    Http.handler(completeRequest) {
      Http.collect[Event[Chunk[Byte]]] {
        case Event.Read(data) => ???
        case Event.Complete   => ???
      }
    }
  }
}
  