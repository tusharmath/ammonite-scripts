package http

import java.nio.Buffer
import zio.ZIO

/**
 * A server typically looks like this
 *
 * Request(method, url, headers), Content(buffer), Content(buffer), LastContent(buffer)
 *
 * |Q...C..C.....C.....L
 *
 * |...A.....C......C......L
 *
 * |Q...C..C.....C.....L
 *
 * |.......................AL
 */

object Http_0 {
  type Method
  type Url
  type Header
  type Status

  sealed trait QMessage
  object QMessage {
    case class Request(method: Method, url: Url, headers: List[Header]) extends QMessage
    case class Content(buffer: Buffer)                                  extends QMessage
    case object Last                                                    extends QMessage
  }

  sealed trait AMessage
  object AMessage {
    case class Response(status: Status, headers: List[Header]) extends AMessage
    case class Content(buffer: Buffer)                         extends QMessage
    case object Last                                           extends QMessage
  }

  sealed trait Event[+A]
  object Event {
    case class Read[A](msg: A)           extends Event[A]
    case class Failure(cause: Throwable) extends Event[Nothing]
    case object Complete                 extends Event[Nothing]
  }

  sealed trait Operation[+A] { self =>
    def ++[A1 >: A](other: Operation[A1]): Operation[A1] = Operation.Combine(self, other)
  }

  object Operation {
    def close: Operation[Nothing]               = Close
    def read: Operation[Nothing]                = Read
    def flush: Operation[Nothing]               = Flush
    def write[D](data: D): Operation[D]         = Write(data)
    def writeAndFlush[D](data: D): Operation[D] = WriteAndFlush(data)

    case class Write[D](data: D)                                   extends Operation[D]
    case class WriteAndFlush[D](data: D)                           extends Operation[D]
    case object Read                                               extends Operation[Nothing]
    case object Flush                                              extends Operation[Nothing]
    case object Close                                              extends Operation[Nothing]
    case class Combine[A](self: Operation[A], other: Operation[A]) extends Operation[A]
  }

  case class Http[R, E, A, B](execute: A => ZIO[R, Option[E], B])

}
