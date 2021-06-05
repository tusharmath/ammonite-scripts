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

  // Message Info
  // type Body
  // type Body

  type Buffered
  type Complete

  /**
   * Extract `Content` from `Request` and `Response`
   */
  sealed trait HasContent[-A] {
    import Response._
    type Out >: A
    def content[R, E, A1 <: Out](request: Request[R, E, A1]): Content[R, E, Out]
    def content[R, E, A1 <: Out](response: Response[R, E, A1]): Content[R, E, Out]
    def status[R, E, A1 <: A](response: Response[R, E, A1]): Status        = response match {
      case Response.Default(status, _, _) => status
      case _                              => throw new Error("status is unavailable")
    }
    def headers[R, E, A1 <: A](response: Response[R, E, A1]): List[Header] = response match {
      case Default(_, headers, _) => headers
      case _                      => throw new Error("headers are unavailable")
    }
  }

  object HasContent {
    import Content._
    implicit case object HasBuffered extends HasContent[Buffered] {
      override type Out = Buffered
      override def content[R, E, A1 <: Out](request: Request[R, E, A1]): Content[R, E, Out]   = request.asDefault.content
      override def content[R, E, A1 <: Out](response: Response[R, E, A1]): Content[R, E, Out] = response.asDefault.content
    }

    implicit case object HasComplete extends HasContent[Complete] {
      override type Out = Complete
      override def content[R, E, A1 <: Out](request: Request[R, E, A1]): Content[R, E, Out]   = request.asDefault.content
      override def content[R, E, A1 <: Out](response: Response[R, E, A1]): Content[R, E, Out] = response.asDefault.content
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
    case object EmptyContent                                               extends Content[Any, Nothing, Nothing]

    def fromByteBuf(data: ByteBuf): Content[Any, Nothing, Complete]             = CompleteContent(data)
    def fromStream[R, E](data: ZStream[R, E, ByteBuf]): Content[R, E, Buffered] = BufferedContent(data)
    def empty: Content[Any, Nothing, Nothing]                                   = EmptyContent
  }

  sealed trait Request[-R, +E, +A] { self =>
    def method: Method
    def url: Url
    def headers: List[Header]

    def widen[A1](implicit ev: A <:< A1): Request[R, E, A1] = ev.liftCo(self)

    def content(implicit ev: HasContent[A]): Content[R, E, ev.Out]                                                                                                   = ev.content(self)
    def asDefault(implicit ev: HasContent[A]): Request.Default[R, E, ev.Out]                                                                                         = self.asInstanceOf[Request.Default[R, E, ev.Out]]
    def copy[R1, E1, A1](method: Method = self.method, url: Url = self.url, headers: List[Header] = self.headers, content: Content[R1, E1, A1]): Request[R1, E1, A1] =
      Request.Default(method, url, headers, content)
  }

  object Request {
    final case class Default[-R, +E, A](method: Method, url: Url, headers: List[Header], content: Content[R, E, A]) extends Request[R, E, A]
    final case class FromJHttpRequest(jReq: JHttpRequest)                                                           extends Request[Any, Nothing, Nothing] {
      override def method: Method        = ???
      override def url: Url              = ???
      override def headers: List[Header] = ???
    }
    def apply[R, E, A](method: Method, url: Url, headers: List[Header], content: Content[R, E, A]): Request[R, E, A] = Default[R, E, A](method, url, headers, content)
  }

  sealed trait Response[-R, +E, +A] { self =>
    def status(implicit ev: HasContent[A]): Status                                                = ev.status(self)
    def headers(implicit ev: HasContent[A]): List[Header]                                         = ev.headers(self)
    def content(implicit ev: HasContent[A]): Content[R, E, ev.Out]                                = ev.content(self)
    def asDefault[R1 <: R, E1 >: E](implicit ev: HasContent[A]): Response.Default[R1, E1, ev.Out] = self.asInstanceOf[Response.Default[R1, E1, ev.Out]]
  }

  object Response {
    final case class Default[R, E, A](status: Status, headers: List[Header], content: Content[R, E, A])        extends Response[R, E, A]
    final case class Socket(server: SocketServer[Any, Nothing])                                                extends Response[Any, Nothing, Nothing]
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

    def apply[R, E, A: IsBuffered](status: Status, headers: List[Header], content: Content[R, E, Buffered]): Response[R, E, Buffered] =
      Default[R, E, Buffered](status, headers, content)
    def apply[R, E, A: IsComplete](status: Status, headers: List[Header], content: Content[R, E, Complete]): Response[R, E, Complete] =
      Default[R, E, Complete](status, headers, content)
  }

  /// E X A M P L E
  case class HttpApp[-R, +E, -A, +B](run: Request[Any, Nothing, A] => ZIO[R, Option[E], Response[R, E, B]]) {
    def complete(implicit ev: Complete <:< A, b: HasContent[B]): HttpApp[R, E, Any, Any] = HttpApp.collect[Any] { req =>
      Response.decodeCompleteM { byteBuf =>
        run(req.copy(content = Content.fromByteBuf(byteBuf)).widen)
      }
    }

    def buffered(implicit ev: Buffered <:< A, b: HasContent[B]): HttpApp[R, E, Any, Any] = HttpApp.collect[Any] { req =>
      Response.decodeBufferedM { stream =>
        run(req.copy(content = Content.fromStream(stream)).widen)
      }
    }

    def ++[R1 <: R, E1 >: E, A1 <: A, B1 >: B](other: HttpApp[R1, E1, A1, B1]): HttpApp[R1, E1, A1, B1] = HttpApp(req =>
      run(req).catchAll(e =>
        e match {
          case Some(value) => ZIO.fail(Option(value))
          case None        => other.run(req)
        },
      ),
    )
  }
  object HttpApp                                                                                            {
    def collect[A]: MkCollect[A] = new MkCollect[A](())

    final class MkCollect[A](val unit: Unit) extends AnyVal {
      def apply[R, E, B](pf: PartialFunction[Request[Any, Nothing, A], Response[R, E, B]]): HttpApp[R, E, A, B] =
        HttpApp(req => if (pf.isDefinedAt(req)) UIO(pf(req)) else ZIO.fail(None))
    }
  }

  val app0 = HttpApp.collect[Complete] { case req =>
    Response(Status.Ok, Nil, req.content)
  }

  val app1 = HttpApp.collect[Buffered] { case req =>
    Response(Status.Ok, Nil, req.content)
  }

  val app2 = HttpApp.collect[Any] { case req =>
    Response(Status.Ok, Nil, Content.fromByteBuf(ByteBuf.fromString("Hello")))
  }

  val app = app0 ++ app1 ++ app2

}
