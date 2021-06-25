import $ivy.`dev.zio::zio:1.0.8`
import $ivy.`dev.zio::zio-streams:1.0.8`

import zio._
import zio.stream._

type Header
type SocketServer[-R, +E]
type ByteBuf
object ByteBuf {
  def fromString(str: String): ByteBuf = ???
}
type JHttpRequest
type JFullHttpRequest
type JHttpResponse
type JFullHttpResponse
type JChannelHandlerContext
type Url

// Method
sealed trait Method
object Method {
  case object Get  extends Method
  case object Post extends Method
}

// Status
sealed trait Status
object Status {
  case object Ok       extends Status
  case object NotFound extends Status
  case object Continue extends Status
}

/// --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

trait Http[-R, +E, -A, +B] {
  def execute(a: A): ZIO[R, E, B]
  def map[C](bc: B => C): Http[R, E, A, C]
  def contramap[X](xa: X => A): Http[R, E, X, B]
  def collect[X](pf: PartialFunction[X, A]): Http[R, E, X, B]
}
type UHttp[-A, +B] = Http[Any, Nothing, A, B]

object Http {
  def collect[A]: MkHttp[A]   = new MkHttp(())
  def collectM[A]: MkHttpM[A] = new MkHttpM(())
  def route[A]: MkRoute[A]    = new MkRoute(())

  final class MkHttp[A](val unit: Unit) extends AnyVal {
    def apply[B](pf: PartialFunction[A, B]): Http[Any, Nothing, A, B] = ???
  }

  final class MkHttpM[A](val unit: Unit) extends AnyVal {
    def apply[R, E, B](pf: PartialFunction[A, ZIO[R, Option[E], B]]): Http[R, E, A, B] = ???
  }

  final class MkRoute[A](val unit: Unit) extends AnyVal {
    def apply[R, E, B](pf: PartialFunction[A, Http[R, Any, E, B]]): Http[R, E, A, B] = ???
  }
}

type HttpChannel[-R, +E, -A, +B] = Http[R, E, Event[A], Operation[R, E, B]]
object HttpChannel {
  def collect[A]: MkHttp[A]   = new MkHttp(())
  def collectM[A]: MkHttpM[A] = new MkHttpM(())
  def route[A]: MkRoute[A]    = new MkRoute(())

  final class MkHttp[A](val unit: Unit) extends AnyVal {
    def apply[R, E, B](pf: PartialFunction[Event[A], Operation[R, E, B]]): HttpChannel[R, E, A, B] = ???
  }

  final class MkHttpM[A](val unit: Unit) extends AnyVal {
    def apply[R, E, B](pf: PartialFunction[Event[A], ZIO[R, Option[E], Operation[R, E, B]]]): HttpChannel[R, E, A, B] = ???
  }

  final class MkRoute[A](val unit: Unit) extends AnyVal {
    def apply[R, E, B](pf: PartialFunction[Event[A], HttpChannel[R, Any, E, Operation[R, E, B]]]): HttpChannel[R, E, A, B] = ???
  }
}

case class Content[A](data: Chunk[A], isLast: Boolean)

trait Codec[A, B] {
  def apply(ctx: JChannelHandlerContext => Any): Unit = update(ctx)
  def update(ctx: JChannelHandlerContext => Any): Unit
}

object Codec {
  implicit def serverCodec[R, E] = new Codec[Event[Request], Operation[R, E, Response]] {
    override def update(ctx: JChannelHandlerContext => Any): Unit = ???
  }

  // could not find implicit value for parameter codec: Codec[Event[Content[zio.Chunk[Byte]]],Operation[Any,Nothing,Content[zio.Chunk[Byte]]]]
  implicit def contentCodec[R, E] = new Codec[Event[Content[Byte]], Operation[R, E, Content[Byte]]] {
    override def update(ctx: JChannelHandlerContext => Any): Unit = ???
  }
}

sealed trait Event[+A]
object Event {
  case class Message[A](msg: A)        extends Event[A]
  case class Failure(cause: Throwable) extends Event[Nothing]
  case object Complete                 extends Event[Nothing]
}

sealed trait Operation[-R, +E, +A] {
  def andThen[R1 <: R, E1 >: E, A1 >: A](other: Operation[R1, E1, A1]): Operation[R1, E1, A1] = ???
}
object Operation                   {
  case object Empty                                                         extends Operation[Any, Nothing, Nothing]
  case class Write[A](msg: A)                                               extends Operation[Any, Nothing, A]
  case class WriteStream[R, E, A](stream: ZStream[R, E, A])                 extends Operation[R, E, A]
  case class Update[R, E, A, B](codec: Codec[A, B], http: Http[R, E, A, B]) extends Operation[R, E, Nothing]
  case object Read                                                          extends Operation[Any, Nothing, Nothing]
  case object Close                                                         extends Operation[Any, Nothing, Nothing]
  case class Effect[R, E](z: ZIO[R, E, Any])                                extends Operation[R, E, Nothing]

  def write[A](a: A): Operation[Any, Nothing, A]                                                     = Operation.Write(a)
  def use[R, E, A, B](http: Http[R, E, A, B])(implicit codec: Codec[A, B]): Operation[R, E, Nothing] = Update(codec, http)
  def empty: Operation[Any, Nothing, Nothing]                                                        = Empty
  def read: Operation[Any, Nothing, Nothing]                                                         = Read
}

sealed trait Request {
  def method: Method
  def url: Url
  def headers: List[Header]
}

sealed trait Response
object Response {
  def apply(status: Status = Status.Ok, headers: List[Header] = Nil): Response = ???
}

def server[R, E](http: Http[R, E, Event[Request], Operation[R, E, Response]]) = ???

// Case 1 -- Simple Request / Response
object SimpleExample {
  import Event._
  val basic = Http.collect[Request](req => Response())
  val app   = basic.map(data => Operation.write(data)).collect[Event[Request]]({ case Message(req) => req })

  server(app)
}

// Case 2 -- Echo
object StreamingExample {
  import Event._
  import Codec._

  val continueWith100: Http[Any, Nothing, Any, Nothing] = ???

  val echo = Http.collect[Event[Content[Byte]]]({
    case Message(content) => Operation.write(content)
    case Complete         => Operation.read
  })

  val app = Http.collect[Event[Request]]({ case Message(req) =>
    ???
  })
  server(app)
}
