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
  type Empty
  type Head

  sealed trait HasHead[-A] {
    // From Response
    def status[R, E, A1 <: A](response: Response[R, E, A1]): Status
    def headers[R, E, A1 <: A](response: Response[R, E, A1]): List[Header]

    // From Request
    def method[R, E, A1 <: A](request: Request[R, E, A1]): Method
    def url[R, E, A1 <: A](request: Request[R, E, A1]): Url
    def headers[R, E, A1 <: A](request: Request[R, E, A1]): List[Header]
  }

  object HasHead {    
    implicit object Head extends HasHead[Head] {
      
      override def status[R, E, A1 <: Head](response: Response[R, E, A1]): Status =
        response match {
          case Response.Default(status, _, _) => status
          case _                              => throw new Error("status is unavailable")
        }

      override def headers[R, E, A1 <: Head](response: Response[R, E, A1]): List[Header] =
        response match {
          case Response.Default(_, headers, _) => headers
          case _                               => throw new Error("headers are unavailable")
        }

      override def method[R, E, A1 <: Head](request: Request[R, E, A1]): Method        = request match {
        case Request.Default(method, _, _, _) => method
        case Request.FromJHttpRequest(jReq)   => ???
      }
      override def url[R, E, A1 <: Head](request: Request[R, E, A1]): Url              = request match {
        case Request.Default(_, url, _, _)  => url
        case Request.FromJHttpRequest(jReq) => ???
      }
      override def headers[R, E, A1 <: Head](request: Request[R, E, A1]): List[Header] = request match {
        case Request.Default(_, _, headers, _) => headers
        case Request.FromJHttpRequest(jReq)    => ???
      }
    }
  }

  /**
   * Extract `Content` from `Request` and `Response`
   */
  sealed trait HasContent[-A] {
    type Out >: A
    def content[R, E, A1 <: Out](request: Request[R, E, A1]): Content[R, E, Out]
    def content[R, E, A1 <: Out](response: Response[R, E, A1]): Content[R, E, Out]
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

  sealed trait IsBuffered[-A]
  implicit object IsBuffered extends IsBuffered[Buffered]

  sealed trait IsComplete[-A]
  implicit object IsComplete extends IsComplete[Complete]

  /**
   * Extracts data from `Content`
   */
  sealed trait HasData[-A] {
    type Out[-R, +E, A]
    def data[R, E, A1 <: A](content: Content[R, E, A1]): Out[R, E, A1]
  }

  object HasData {
    import Content._
    implicit case object Complete extends HasData[Complete] {
      override type Out[-R, +E, A] = ByteBuf
      override def data[R, E, A1 <: Complete](content: Content[R, E, A1]): Out[R, E, A1] = content match {
        case CompleteContent(bytes) => bytes
        case _                      => throw new Error("Data is Unavailable")
      }
    }
    implicit case object Buffered extends HasData[Buffered] {
      override type Out[-R, +E, A] = ZStream[R, E, ByteBuf]
      override def data[R, E, A1 <: Buffered](content: Content[R, E, A1]): Out[R, E, A1] = content match {
        case BufferedContent(source) => source
        case _                       => throw new Error("Data is Unavailable")
      }
    }
  }

  sealed trait Content[-R, +E, A] { self =>
    def data(implicit ev: HasData[A]): ev.Out[R, E, A] = ev.data(self)
  }
  object Content                  {
    final case class CompleteContent(bytes: ByteBuf)                       extends Content[Any, Nothing, Complete]
    final case class BufferedContent[R, E](source: ZStream[R, E, ByteBuf]) extends Content[R, E, Buffered]
    case object EmptyContent                                               extends Content[Any, Nothing, Empty]

    def fromByteBuf(data: ByteBuf): Content[Any, Nothing, Complete]             = CompleteContent(data)
    def fromStream[R, E](data: ZStream[R, E, ByteBuf]): Content[R, E, Buffered] = BufferedContent(data)
    def empty: Content[Any, Nothing, Empty]                                     = EmptyContent
  }

  sealed trait Request[-R, +E, A] { self =>
    def method(implicit ev: HasHead[A]): Method                              = ev.method(self)
    def url(implicit ev: HasHead[A]): Url                                    = ev.url(self)
    def headers(implicit ev: HasHead[A]): List[Header]                       = ev.headers(self)
    def content(implicit ev: HasContent[A]): Content[R, E, ev.Out]           = ev.content(self)
    def asDefault(implicit ev: HasContent[A]): Request.Default[R, E, ev.Out] = self.asInstanceOf[Request.Default[R, E, ev.Out]]
  }

  object Request {
    final case class Default[-R, +E, A](method: Method, url: Url, headers: List[Header], content: Content[R, E, A]) extends Request[R, E, A with Head]
    final case class FromJHttpRequest(jReq: JHttpRequest)                                                           extends Request[Any, Nothing, Head]
    def apply[R, E, A](method: Method, url: Url, headers: List[Header], content: Content[R, E, A]): Request[R, E, A with Head] = Default[R, E, A](method, url, headers, content)
  }

  sealed trait Response[-R, +E, A] { self =>
    def status(implicit ev: HasHead[A]): Status                               = ev.status(self)
    def headers(implicit ev: HasHead[A]): List[Header]                        = ev.headers(self)
    def content(implicit ev: HasContent[A]): Content[R, E, ev.Out]            = ev.content(self)
    def asDefault(implicit ev: HasContent[A]): Response.Default[R, E, ev.Out] = self.asInstanceOf[Response.Default[R, E, ev.Out]]
  }

  object Response {
    final case class Default[-R, +E, A](status: Status, headers: List[Header], content: Content[R, E, A]) extends Response[R, E, A with Head]
    final case class Socket(server: SocketServer[Any, Nothing])                                           extends Response[Any, Nothing, Empty]
    final case class Decode[R, E, A, B, C](d: Decoder[A, B], cb: B => Response[R, E, C])                  extends Response[R, E, Empty]

    /**
     * Used to decode request body
     */
    sealed trait Decoder[-A, B]
    object Decoder {
      implicit object DecodeComplete extends Decoder[Complete, ByteBuf]
      implicit object DecodeBuffered extends Decoder[Buffered, ZStream[Any, Nothing, ByteBuf]]
    }

    def decode[R, E, A, B, C: HasContent](decoder: Decoder[A, B])(cb: B => Response[R, E, C]): Response[R, E, Empty] = Decode(decoder, cb)

    def apply[R, E, A: IsBuffered](status: Status, headers: List[Header], content: Content[R, E, Buffered]): Response[R, E, Buffered with Head] =
      Default[R, E, Buffered](status, headers, content)
    def apply[R, E, A: IsComplete](status: Status, headers: List[Header], content: Content[R, E, Complete]): Response[R, E, Complete with Head] =
      Default[R, E, Complete](status, headers, content)
  }

  /// E X A M P L E
  case class HttpApp[-R, +E, A, B](run: Request[Any, Nothing, A with Head] => ZIO[R, Option[E], Response[R, E, B]])
  object HttpApp {
    def collect[A]: MkCollect[A] = new MkCollect[A](())
    final class MkCollect[A](val unit: Unit) extends AnyVal {
      def apply[R, E, B](pf: PartialFunction[Request[Any, Nothing, A with Head], Response[R, E, B]]): HttpApp[R, E, A, B] =
        HttpApp(req => if (pf.isDefinedAt(req)) UIO(pf(req)) else ZIO.fail(None))
    }
  }

  val app = HttpApp.collect[Complete] { case req =>
    val content = req.content
    Response(Status.Ok, Nil, content)
  }

  val app1 = HttpApp.collect[Empty] { case req =>
    Response(Status.Ok, Nil, Content.fromByteBuf(ByteBuf.fromString("Hello")))
  }

  val app0: HttpApp[Any, Nothing, Buffered, Buffered with Head] = HttpApp.collect[Buffered] { case req =>
    Response(Status.Ok, Nil, req.content)
  }
}
