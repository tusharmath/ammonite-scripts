package response

import zio.stream.ZStream
import zio.ZIO

object Response_16 {

  type Header
  type SocketServer[-R, +E]
  type ByteBuf
  type JHttpRequest
  type JFullHttpRequest
  type JHttpResponse
  type JFullHttpResponse
  type JChannelHandler
  type Url
  type Buffered
  type Complete
  type Opaque
  type UHttp[-A, +B]               = Http[Any, Nothing, A, B]
  type HttpChannel[-R, +E, -A, +B] = Http[R, E, Event[A], Operation[R, E, B]]

/// --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

// Method
  sealed trait Method

// Status
  sealed trait Status

  /**
   * Extract `Content` from `Request` and `Response`
   */
  sealed trait HasContent[-A] {
    type Out >: A

    def content[R, E, A1 <: Out](request: Request[R, E, A1]): Content[R, E, Out]   =
      request match {
        case Request.Default(_, _, _, dContent) => dContent
        case _                                  => throw InvalidAccess("content", request)
      }
    def content[R, E, A1 <: Out](response: Response[R, E, A1]): Content[R, E, Out] =
      response match {
        case Response.Default(_, _, dContent) => dContent
        case _                                => throw InvalidAccess("content", response)
      }
    def status[R, E, A1 <: A](response: Response[R, E, A1]): Status                = response match {
      case Response.Default(status, _, _) => status
      case _                              => throw InvalidAccess("status", response)
    }
    def headers[R, E, A1 <: A](response: Response[R, E, A1]): List[Header]         = response match {
      case Response.Default(_, headers, _) => headers
      case _                               => throw InvalidAccess("headers", response)
    }
  }

  /**
   * Extracts data from `Content`
   */
  sealed trait HasData[-A] {
    type Out[-R, +E]
    def data[R, E, A1 <: A](content: Content[R, E, A1]): Out[R, E]
  }

  sealed trait Content[-R, +E, +A] { self =>
    def data[A1 >: A](implicit ev: HasData[A1]): ev.Out[R, E] = ev.data(self)
  }

  sealed trait Event[+A]

  sealed trait Operation[-R, +E, +D] { self =>
    def ++[R1 <: R, E1 >: E, D1 >: D](other: Operation[R1, E1, D1]): Operation[R1, E1, D1] = self andThen other

    def andThen[R1 <: R, E1 >: E, D1 >: D](other: Operation[R1, E1, D1]): Operation[R1, E1, D1] = ???
  }

  sealed trait Request[-R, +E, A] { self =>
    def method: Method
    def url: Url
    def headers: List[Header]

    def content(implicit ev: HasContent[A]): Content[R, E, ev.Out] = ev.content(self)

    def copy[R1, E1, A1](method: Method = self.method, url: Url = self.url, headers: List[Header] = self.headers, content: Content[R1, E1, A1]): Request[R1, E1, A1] =
      Request.Default(method, url, headers, content)
  }

  sealed trait Response[-R, +E, A] { self =>
    def status(implicit ev: HasContent[A]): Status                 = ev.status(self)
    def headers(implicit ev: HasContent[A]): List[Header]          = ev.headers(self)
    def content(implicit ev: HasContent[A]): Content[R, E, ev.Out] = ev.content(self)
  }

  /**
   * Used to decode request body
   */
  sealed trait DecodeMap[A, B] {
    val channelHandler: JChannelHandler
  }

  trait Http[-R, +E, -A, +B] {
    def execute(a: A): ZIO[R, E, B]
    def map[C](bc: B => C): Http[R, E, A, C]
    def contramap[X](xa: X => A): Http[R, E, X, B]
    def collect[X](pf: PartialFunction[X, A]): Http[R, E, X, B]
    def +++[R1 <: R, E1 >: E, A1 <: A, B1 >: B](other: Http[R1, E1, A1, B1]): Http[R1, E1, A1, B1] = ???
  }

  final case class InvalidAccess(property: String, obj: Any) extends Throwable {
    override def getMessage(): String = s"""The property "$property" is unavailable on: $obj"""
  }

  object ByteBuf {
    def fromString(str: String): ByteBuf = ???
  }

  object Method {
    case object Get  extends Method
    case object Post extends Method
  }

  object Status {
    case object Ok       extends Status
    case object NotFound extends Status
  }

  object Http {
    def collect[A]: MkHttp[A]   = new MkHttp()
    def collectM[A]: MkHttpM[A] = new MkHttpM()
    def route[A]: MkRoute[A]    = new MkRoute()

    final class MkHttp[A]() {
      def apply[B](pf: PartialFunction[A, B]): Http[Any, Nothing, A, B] = ???
    }

    final class MkHttpM[A]() {
      def apply[R, E, B](pf: PartialFunction[A, ZIO[R, Option[E], B]]): Http[R, E, A, B] = ???
    }

    final class MkRoute[A]() {
      def apply[R, E, B](pf: PartialFunction[A, Http[R, Any, E, B]]): Http[R, E, A, B] = ???
    }
  }

  object HasContent {
    implicit case object HasNothing  extends HasContent[Opaque]   {
      override type Out = Any
    }
    implicit case object HasBuffered extends HasContent[Buffered] {
      override type Out = Buffered
    }
    implicit case object HasComplete extends HasContent[Complete] {
      override type Out = Complete
    }
  }

  object HasData {
    import Content._
    implicit case object Complete extends HasData[Complete] {
      override type Out[-R, +E] = ByteBuf
      override def data[R, E, A1 <: Complete](content: Content[R, E, A1]): Out[R, E] = content match {
        case CompleteContent(bytes) => bytes
        case _                      => throw new Error("Data is Unavailable")
      }
    }
    implicit case object Buffered extends HasData[Buffered] {
      override type Out[-R, +E] = ZStream[R, E, ByteBuf]
      override def data[R, E, A1 <: Buffered](content: Content[R, E, A1]): Out[R, E] = content match {
        case BufferedContent(source) => source
        case _                       => throw new Error("Data is Unavailable")
      }
    }
  }

  object Content {
    def fromByteBuf(data: ByteBuf): Content[Any, Nothing, Complete] = CompleteContent(data)

    def fromStream[R, E](data: ZStream[R, E, ByteBuf]): Content[R, E, Buffered] = BufferedContent(data)

    def empty: Content[Any, Nothing, Opaque] = EmptyContent

    def collect[A]: MkContent[A] = ???

    final case class CompleteContent(bytes: ByteBuf) extends Content[Any, Nothing, Complete]

    final case class BufferedContent[R, E](source: ZStream[R, E, ByteBuf]) extends Content[R, E, Buffered]

    final case class ContentHandler[R, A, B](d: DecodeMap[A, B], ch: HttpChannel[R, Nothing, A, B]) extends Content[R, Nothing, Opaque]

    final class MkContent[A]() {
      def apply[R, E, B](pf: PartialFunction[Event[A], Operation[R, Nothing, B]])(implicit d: DecodeMap[A, B]): Content[R, Nothing, Opaque] =
        ContentHandler(d, HttpChannel.collect(pf))
    }

    case object EmptyContent extends Content[Any, Nothing, Opaque]
  }

  object Event {
    case class Read[A](msg: A, isLast: Boolean) extends Event[A]

    case class Failure(cause: Throwable) extends Event[Nothing]

    case object Complete extends Event[Nothing]
  }

  object Operation {
    def empty: Operation[Any, Nothing, Nothing] = Empty

    def close: Operation[Any, Nothing, Nothing] = Empty

    def read: Operation[Any, Nothing, Nothing] = Read

    def write[D](data: D): Operation[Any, Nothing, D] = Write(data)

    def fromEffect[R, E](effect: ZIO[R, E, Any]): Operation[R, E, Any] = FromEffect(effect)

    def updateChannel[R, A, B](ch: HttpChannel[R, Nothing, A, B])(implicit d: DecodeMap[A, B]): Operation[R, Nothing, Nothing] = Update(d, ch)

    case class FromEffect[R, E](effect: ZIO[R, E, Any]) extends Operation[R, E, Nothing]

    case class Write[D](data: D) extends Operation[Any, Nothing, D]

    case class Update[R, E, A, B](d: DecodeMap[A, B], ch: HttpChannel[R, Nothing, A, B]) extends Operation[R, E, Nothing]

    case object Read extends Operation[Any, Nothing, Nothing]

    case object Close extends Operation[Any, Nothing, Nothing]

    case object Empty extends Operation[Any, Nothing, Nothing]
  }

  object Request {
    def apply[R, E, A](method: Method, url: Url, headers: List[Header], content: Content[R, E, A]): Request[R, E, A] = Default[R, E, A](method, url, headers, content)

    final case class Default[R, E, A](method: Method, url: Url, headers: List[Header], dContent: Content[R, E, A]) extends Request[R, E, A]

    final case class FromJHttpRequest(jReq: JHttpRequest) extends Request[Any, Nothing, Nothing] {
      override def method: Method        = ???
      override def url: Url              = ???
      override def headers: List[Header] = ???
    }
  }

  object Response {
    def apply[R, E, A](status: Status = Status.Ok, headers: List[Header] = Nil, content: Content[R, E, A] = Content.empty): Response[R, E, A] =
      Default(status, headers, content)

    final case class Default[R, E, A](dStatus: Status, dHeaders: List[Header], dContent: Content[R, E, A]) extends Response[R, E, A]

    final case class Socket(server: SocketServer[Any, Nothing]) extends Response[Any, Nothing, Opaque]
  }

  object HttpChannel {
    def collect[A]: MkHttpChannel[A] = MkHttpChannel()
    final case class MkHttpChannel[A]() {
      def apply[R, E, B](pf: PartialFunction[Event[A], Operation[R, E, B]]): HttpChannel[R, E, A, B] = Http.collect[Event[A]](pf)
    }
  }

  object DecodeMap {
    implicit val raw = new DecodeMap[ByteBuf, ByteBuf] {
      override val channelHandler: JChannelHandler = ???
    }
  }

  object Example {
    import Event._
    val echo = Response(
      status = Status.Ok,
      content = Content.collect[ByteBuf] {
        case Read(msg, isLast) => Operation.write(msg) ++ (if (isLast) Operation.close else Operation.empty)
        case Complete          => Operation.read
      },
    )
  }
}
