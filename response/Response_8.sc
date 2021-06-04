import scala.annotation.implicitNotFound
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
      override def status[R, E, A1 <: Head](response: Response[R, E, A1]): Status        = ???
      override def headers[R, E, A1 <: Head](response: Response[R, E, A1]): List[Header] = ???
      override def method[R, E, A1 <: Head](request: Request[R, E, A1]): Method          = ???
      override def url[R, E, A1 <: Head](request: Request[R, E, A1]): Url                = ???
      override def headers[R, E, A1 <: Head](request: Request[R, E, A1]): List[Header]   = ???
    }
  }

  sealed trait HasContent[-A]
  object HasContent {
    implicit case object HasBuffered extends HasContent[Buffered]
    implicit case object HasComplete extends HasContent[Complete]
  }

  sealed trait IsBuffered[-A]
  implicit object IsBuffered extends IsBuffered[Buffered]

  sealed trait IsComplete[-A]
  implicit object IsComplete extends IsComplete[Complete]

  sealed trait Decoder[-A, B]

  object Decoder {
    implicit case object Complete extends Decoder[Complete, ByteBuf]
    implicit case object Buffered extends Decoder[Buffered, ZStream[Any, Nothing, ByteBuf]]

    def complete: Decoder[Complete, ByteBuf]                        = Complete
    def buffered: Decoder[Buffered, ZStream[Any, Nothing, ByteBuf]] = Buffered
  }

  sealed trait Content[-R, +E, A] { self =>
    def widen[R1 <: R, E1 >: E, A1](implicit ev: A <:< A1): Content[R1, E1, A1]  = self.asInstanceOf[Content[R1, E1, A1]]
    def narrow[R1 <: R, E1 >: E, A1](implicit ev: A1 <:< A): Content[R1, E1, A1] = self.asInstanceOf[Content[R1, E1, A1]]
  }
  object Content                  {
    final case class CompleteContent(bytes: ByteBuf)                       extends Content[Any, Nothing, Complete]
    final case class BufferedContent[R, E](source: ZStream[R, E, ByteBuf]) extends Content[R, E, Buffered]
    case object EmptyContent                                               extends Content[Any, Nothing, Empty]

    def apply(data: ByteBuf): Content[Any, Nothing, Complete]              = CompleteContent(data)
    def apply[R, E](data: ZStream[R, E, ByteBuf]): Content[R, E, Buffered] = BufferedContent(data)

    def complete(data: ByteBuf): Content[Any, Nothing, Complete]              = CompleteContent(data)
    def buffered[R, E](data: ZStream[R, E, ByteBuf]): Content[R, E, Buffered] = BufferedContent(data)
    def empty: Content[Any, Nothing, Empty]                                   = EmptyContent
  }

  sealed trait Request[-R, +E, A] { self =>
    def method(implicit ev: HasHead[A]): Method        = ???
    def url(implicit ev: HasHead[A]): Url              = ???
    def headers(implicit ev: HasHead[A]): List[Header] = ???
    def content[B](implicit d: Decoder[A, B]): B       = ???

    def widen[R1 <: R, E1 >: E, A1](implicit ev: A <:< A1): Request[R1, E1, A1] = self.asInstanceOf[Request[R1, E1, A1]]
  }

  object Request {
    final case class Default[R, E, A](method: Method, url: Url, headers: List[Header], content: Content[R, E, A]) extends Request[R, E, A with Head]
    final case class FromJHttpRequest(jReq: JHttpRequest)                                                         extends Request[Any, Nothing, Head]
    def apply[R, E, A](method: Method, url: Url, headers: List[Header], content: Content[R, E, A]): Request[R, E, A with Head] = Default[R, E, A](method, url, headers, content)
  }

  sealed trait Response[-R, +E, A] { self =>
    def status[A1 <: A](implicit h: HasHead[A1]): Status        = ???
    def headers[A1 <: A](implicit h: HasHead[A1]): List[Header] = ???
    def content[A1 <: A, X](implicit d: Decoder[A1, X]): X      = ???
  }

  object Response {
    final case class Default[R, E, A](status: Status, headers: List[Header], content: Content[R, E, A]) extends Response[R, E, A with Head]
    final case class Socket(server: SocketServer[Any, Nothing])                                         extends Response[Any, Nothing, Empty]
    final case class Decode[R, E, A, B, C](d: Decoder[A, B], cb: B => Response[R, E, C])                extends Response[R, E, Empty]

    def decode[R, E, A, B, C: HasContent](decoder: Decoder[A, B])(cb: B => Response[R, E, C]): Response[R, E, Empty]                            = Decode(decoder, cb)
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

  val app: HttpApp[Any, Nothing, Complete, Complete with Head] = HttpApp.collect[Complete] { case req =>
    println(req.url)
    println(req.content)
    val content = Content(req.content)
    (Response(Status.Ok, Nil, content))
  }

  val app0: HttpApp[Any, Nothing, Buffered, Buffered with Head] = HttpApp.collect[Buffered] { case req =>
    println(req.url)
    println(req.content)
    val content = Content(req.content)
    (Response(Status.Ok, Nil, content))
  }
}
