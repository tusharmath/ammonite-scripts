package response

object Response_7 {
  import zio._
  import zio.stream._

  type Header
  type SocketServer[-R, +E]
  type ByteBuf
  type JHttpRequest
  type JFullHttpRequest
  type JHttpResponse
  type JFullHttpResponse
  type Url
  type Buffered
  type Complete
  type Opaque
  type Head

  // Message Info
  // type Body
  // type Body

  // Method
  sealed trait Method

  // Status
  sealed trait Status

  sealed trait HasHead[-A] {
    // From Response
    def status[R, E, A1 <: A](response: Response[R, E, A1]): Status
    def headers[R, E, A1 <: A](response: Response[R, E, A1]): List[Header]

    // From Request
    def method[R, E, A1 <: A](request: Request[R, E, A1]): Method
    def url[R, E, A1 <: A](request: Request[R, E, A1]): Url
    def headers[R, E, A1 <: A](request: Request[R, E, A1]): List[Header]
  }

  sealed trait HasContent[-A]

  sealed trait Decoder[-A, B]

  sealed trait Content[-R, +E, A]

  sealed trait Request[-R, +E, A] { self =>
    def method(implicit ev: HasHead[A]): Method        = ???
    def url(implicit ev: HasHead[A]): Url              = ???
    def headers(implicit ev: HasHead[A]): List[Header] = ???
    def content[B](implicit d: Decoder[A, B]): B       = ???

    def widen[R1 <: R, E1 >: E, A1](implicit ev: A <:< A1): Request[R1, E1, A1] = self.asInstanceOf[Request[R1, E1, A1]]
  }

  sealed trait Response[-R, +E, A] { self =>
    def status[A1 <: A](implicit h: HasHead[A1]): Status        = ???
    def headers[A1 <: A](implicit h: HasHead[A1]): List[Header] = ???
    def content[A1 <: A, X](implicit d: Decoder[A1, X]): X      = ???
  }

  /// E X A M P L E
  // // val a = Response.decodeA[Body].apply(x => Response.Head(Status.Ok, Nil, Content.Full(x)))
  // val a = Response.decodeComplete(x => Response.Default(Status.Ok, Nil, Content.data(x)))
  case class HttpApp[-R, +E, A, B](run: Request[Any, Nothing, A with Head] => ZIO[R, Option[E], Response[R, E, B]])

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

  object HasHead {
    implicit object Head extends HasHead[Head] {
      override def status[R, E, A1 <: Head](response: Response[R, E, A1]): Status        = ???
      override def headers[R, E, A1 <: Head](response: Response[R, E, A1]): List[Header] = ???
      override def method[R, E, A1 <: Head](request: Request[R, E, A1]): Method          = ???
      override def url[R, E, A1 <: Head](request: Request[R, E, A1]): Url                = ???
      override def headers[R, E, A1 <: Head](request: Request[R, E, A1]): List[Header]   = ???
    }
  }

  object HasContent {
    implicit case object HasBuffered extends HasContent[Buffered]
    implicit case object HasComplete extends HasContent[Complete]
  }

  object Decoder {
    implicit case object Complete extends Decoder[Complete, ByteBuf]
    implicit case object Buffered extends Decoder[Buffered, ZStream[Any, Nothing, ByteBuf]]

    def complete: Decoder[Complete, ByteBuf]                        = Complete
    def buffered: Decoder[Buffered, ZStream[Any, Nothing, ByteBuf]] = Buffered
  }

  object Content {
    def apply(data: ByteBuf): Content[Any, Nothing, Complete] = CompleteContent(data)

    def apply[R, E](data: ZStream[R, E, ByteBuf]): Content[R, E, Buffered] = BufferedContent(data)

    def complete(data: ByteBuf): Content[Any, Nothing, Complete] = CompleteContent(data)

    def buffered[R, E](data: ZStream[R, E, ByteBuf]): Content[R, E, Buffered] = BufferedContent(data)

    final case class CompleteContent(bytes: ByteBuf) extends Content[Any, Nothing, Complete]

    final case class BufferedContent[R, E](source: ZStream[R, E, ByteBuf]) extends Content[R, E, Buffered]
  }

  object Request {
    def apply[R, E, A](method: Method, url: Url, headers: List[Header], content: Content[R, E, A]): Request[R, E, A with Head] = Default[R, E, A](method, url, headers, content)

    final case class Default[R, E, A](method: Method, url: Url, headers: List[Header], content: Content[R, E, A]) extends Request[R, E, A with Head]

    final case class FromJHttpRequest(jReq: JHttpRequest) extends Request[Any, Nothing, Head]
  }

  object Response {
    def decode[R, E, A, B, C: HasContent](decoder: Decoder[A, B])(cb: B => Response[R, E, C]): Response[R, E, Opaque] = Decode(decoder, cb)

    def apply[R, E, A](status: Status, headers: List[Header], content: Content[R, E, A]): Response[R, E, A with Head] = Default[R, E, A](status, headers, content)

    final case class Default[R, E, A](status: Status, headers: List[Header], content: Content[R, E, A]) extends Response[R, E, A with Head]

    final case class Socket(server: SocketServer[Any, Nothing]) extends Response[Any, Nothing, Opaque]

    final case class Decode[R, E, A, B, C](d: Decoder[A, B], cb: B => Response[R, E, C]) extends Response[R, E, Opaque]
  }

  object HttpApp {
    def collect[A]: MkCollect[A] = new MkCollect[A](())
    final class MkCollect[A](val unit: Unit) extends AnyVal {
      def apply[R, E, B](pf: PartialFunction[Request[Any, Nothing, A with Head], Response[R, E, B]]): HttpApp[R, E, A, B] =
        HttpApp(req => if (pf.isDefinedAt(req)) UIO(pf(req)) else ZIO.fail(None))
    }

    // def decodeComplete[R, E, B](app: HttpApp[R, E, Complete, B]): HttpApp[R, E, O]
  }

  // def program(req: Request[Any, Nothing, Any]) = {
  //   collectComplete(req)(reqWithBody => app(reqWithBody))
  // }

  val app = HttpApp.collect[Complete] { case req =>
    println(req.url)
    println(req.content)
    Response(Status.Ok, Nil, Content(ZStream.succeed(req.content)))
  }

}
