// scala 2.13.6
// Ammonite 2.3.8-67-4b6c67db

import $ivy.`dev.zio::zio:1.0.8`
import $ivy.`dev.zio::zio-streams:1.0.8`

object Response_8 {
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
  }

  /// --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

  type Buffered
  type Complete
  type Opaque

  final case class InvalidAccess(property: String, obj: Any) extends Throwable {
    override def getMessage(): String = s"""The property "$property" is unavailable on: $obj"""
  }

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

  /**
   * Extracts data from `Content`
   */
  sealed trait HasData[-A] {
    type Out[-R, +E]
    def data[R, E, A1 <: A](content: Content[R, E, A1]): Out[R, E]
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

  sealed trait Content[-R, +E, +A] { self =>
    def data[A1 >: A](implicit ev: HasData[A1]): ev.Out[R, E] = ev.data(self)
  }

  object Content {
    final case class CompleteContent(bytes: ByteBuf)                       extends Content[Any, Nothing, Complete]
    final case class BufferedContent[R, E](source: ZStream[R, E, ByteBuf]) extends Content[R, E, Buffered]
    case object EmptyContent                                               extends Content[Any, Nothing, Opaque]

    def fromByteBuf(data: ByteBuf): Content[Any, Nothing, Complete]             = CompleteContent(data)
    def fromStream[R, E](data: ZStream[R, E, ByteBuf]): Content[R, E, Buffered] = BufferedContent(data)
    def empty: Content[Any, Nothing, Opaque]                                    = EmptyContent
  }

  sealed trait Request[-R, +E, +A] { self =>
    def method: Method
    def url: Url
    def headers: List[Header]

    def widen[A1](implicit ev: A <:< A1): Request[R, E, A1] = ev.liftCo(self)

    def content(implicit ev: HasContent[A]): Content[R, E, ev.Out] = ev.content(self)

    def copy[R1, E1, A1](method: Method = self.method, url: Url = self.url, headers: List[Header] = self.headers, content: Content[R1, E1, A1]): Request[R1, E1, A1] =
      Request.Default(method, url, headers, content)
  }

  object Request {
    final case class Default[R, E, A](method: Method, url: Url, headers: List[Header], dContent: Content[R, E, A]) extends Request[R, E, A]
    final case class FromJHttpRequest(jReq: JHttpRequest)                                                          extends Request[Any, Nothing, Nothing] {
      override def method: Method        = ???
      override def url: Url              = ???
      override def headers: List[Header] = ???
    }
    def apply[R, E, A](method: Method, url: Url, headers: List[Header], content: Content[R, E, A]): Request[R, E, A] = Default[R, E, A](method, url, headers, content)
  }

  sealed trait Response[-R, +E, +A] { self =>
    def status(implicit ev: HasContent[A]): Status                 = ev.status(self)
    def headers(implicit ev: HasContent[A]): List[Header]          = ev.headers(self)
    def content(implicit ev: HasContent[A]): Content[R, E, ev.Out] = ev.content(self)
  }

  object Response {
    final case class Default[R, E, A](dStatus: Status, dHeaders: List[Header], dContent: Content[R, E, A]) extends Response[R, E, A]
    final case class Socket(server: SocketServer[Any, Nothing])                                            extends Response[Any, Nothing, Opaque]

    final case class Decode[R, E, A, B, C](d: DecodeMap[A, B], cb: B => Response[R, E, C])                     extends Response[R, E, Nothing]
    final case class DecodeM[R, E, A, B, C](d: DecodeMap[A, B], cb: B => ZIO[R, Option[E], Response[R, E, C]]) extends Response[R, E, Nothing]

    /**
     * Used to decode request body
     */
    sealed trait DecodeMap[-A, B]
    object DecodeMap {
      implicit object DecodeComplete extends DecodeMap[Complete, ByteBuf]
      implicit object DecodeBuffered extends DecodeMap[Buffered, ZStream[Any, Nothing, ByteBuf]]
    }

    sealed trait IsBuffered[-A]
    implicit object IsBuffered extends IsBuffered[Buffered]

    sealed trait IsComplete[-A]
    implicit object IsComplete extends IsComplete[Complete]

    def decode[R, E, A, B, C: HasContent](decoder: DecodeMap[A, B])(cb: B => Response[R, E, C]): Response[R, E, Nothing]                          = Decode(decoder, cb)
    def decodeComplete[R, E, C: HasContent](cb: ByteBuf => Response[R, E, C]): Response[R, E, Nothing]                                            = Decode(DecodeMap.DecodeComplete, cb)
    def decodeBuffered[R, E, C: HasContent](cb: ZStream[Any, Nothing, ByteBuf] => Response[R, E, C]): Response[R, E, Nothing]                     = Decode(DecodeMap.DecodeBuffered, cb)
    def decodeCompleteM[R, E, C: HasContent](cb: ByteBuf => ZIO[R, Option[E], Response[R, E, C]]): Response[R, E, Nothing]                        = DecodeM(DecodeMap.DecodeComplete, cb)
    def decodeBufferedM[R, E, C: HasContent](cb: ZStream[Any, Nothing, ByteBuf] => ZIO[R, Option[E], Response[R, E, C]]): Response[R, E, Nothing] =
      DecodeM(DecodeMap.DecodeBuffered, cb)

    def apply[R, E, A](status: Status = Status.Ok, headers: List[Header] = Nil, content: Content[R, E, A] = Content.empty): Response[R, E, A] =
      Default(status, headers, content)
  }

  /// EXAMPLE
  case class HttpApp[-R, +E](run: Request[Any, Nothing, Nothing] => ZIO[R, Option[E], Response[R, E, Any]]) {
    def ++[R1 <: R, E1 >: E](other: HttpApp[R1, E1]): HttpApp[R1, E1] = HttpApp { req =>
      run(req).catchAll {
        case Some(value) => ZIO.fail(Option(value))
        case None        => other.run(req)
      }
    }
  }

  object HttpApp {
    def collect[R, E, B: HasContent](pf: PartialFunction[Request[Any, Nothing, Nothing], Response[R, E, B]]): HttpApp[R, E] =
      HttpApp(req => if (pf.isDefinedAt(req)) UIO(pf(req)) else ZIO.fail(None))

    def collectComplete[R, E, B: HasContent](pf: PartialFunction[Request[Any, Nothing, Complete], Response[R, E, B]]): HttpApp[R, E] =
      HttpApp { req =>
        UIO {
          Response.decodeComplete { byteBuf =>
            pf(req.copy(content = Content.fromByteBuf(byteBuf)))
          }
        }
      }

    def collectBuffered[R, E, B: HasContent](pf: PartialFunction[Request[Any, Nothing, Buffered], Response[R, E, B]]): HttpApp[R, E] =
      HttpApp { req =>
        UIO {
          Response.decodeBuffered { stream =>
            pf(req.copy(content = Content.fromStream(stream)))
          }
        }
      }
  }

  val app0 = HttpApp.collectComplete { case req =>
    Response(content = req.content)
  }

  val app1 = HttpApp.collectBuffered { case req =>
    Response(content = req.content)
  }

  val app2 = HttpApp.collect { case req =>
    Response(content = Content.fromByteBuf(ByteBuf.fromString("Hello")))
  }

  val app3 = HttpApp.collect { case req =>
    Response(Status.Ok, Nil)
  }

  val app4 = HttpApp.collect { case req =>
    Response.Socket(???)
  }

  val app = app0 ++ app1 ++ app2 ++ app3 ++ app4
}
