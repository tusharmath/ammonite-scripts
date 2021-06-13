import $ivy.`dev.zio::zio:1.0.8`

object Response_2 {

  import zio._

  /**
   * Create a response domain that can be used on both — the clients and the server
   */

  type Header
  type SocketServer[-R, +E]

  type ByteBuf
  object ByteBuf {
    def fromString(str: String): ByteBuf = ???
  }
  type Queue[-R, +E, +A]
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

  // HttpApp
  case class Http[-R, +E, -A, +B](run: A => ZIO[R, Option[E], B])
  object Http    {
    def collect[A] = MkCollect[A](())

    final case class MkCollect[A](val unit: Unit) extends AnyVal {
      def apply[B](pf: PartialFunction[A, B]): Http[Any, Nothing, A, B] = Http(a => if (pf.isDefinedAt(a)) UIO(pf(a)) else ZIO.fail(None))
    }
  }
  case class HttpApp[R, E, A, B](asHttp: Http[R, E, Request[R, E, Request[R, E, A]], Response[R, E, B]]) extends AnyVal {}
  object HttpApp {
    def collect[R, E](pf: PartialFunction[Request[Any, Nothing, MessageHeader], Response[R, E, MessageHeader with MessageBody]]) =
      Http.collect[Request[Any, Nothing, MessageHeader with MessageBody]] {
        case m if pf.isDefinedAt(m) => pf(m)
      }

    def collectM[R, E](pf: PartialFunction[Request[Any, Nothing, MessageHeader], ZIO[R, E, Response[R, E, MessageHeader with MessageBody]]]) =
      ???
  }

  // Type of content
  type MessageHeader
  type CompleteBody
  type QueuedBody
  // sealed trait Opaque // Nothing is accessible
  // sealed trait Partial  extends Opaque  // Everything except content is accessible
  // sealed trait Complete extends Partial // Everything is accessible

  sealed trait Content[-R, +E]
  object Content {
    case object Empty                                    extends Content[Any, Nothing]
    case class Complete(bytes: ByteBuf)                  extends Content[Any, Nothing]
    case class Queued[R, E](queue: Queue[R, E, ByteBuf]) extends Content[R, E]

    def fromByteBuf(bytes: ByteBuf): Content[Any, Nothing]          = Complete(bytes)
    def fromQueue[R, E](queue: Queue[R, E, ByteBuf]): Content[R, E] = Queued(queue)
    def fromString(text: String): Content[Any, Nothing]             = ???
    def empty: Content[Any, Nothing]                                = Empty
  }

  sealed trait Decoder[+A]
  object Decoder {
    case object AsQueue    extends Decoder[Queue[Any, Nothing, ByteBuf]]
    case object AsComplete extends Decoder[ByteBuf]
    def asQueue[R, E]: Decoder[Queue[R, E, ByteBuf]] = AsQueue
    def asComplete: Decoder[ByteBuf]                 = AsComplete
  }

  sealed trait Response[-R, +E, +A]
  object Response {
    case class Head(status: Status, headers: List[Header])                                                                  extends Response[Any, Nothing, MessageHeader]
    case class Socket[R, E](app: SocketServer[R, E])                                                                        extends Response[R, E, Nothing]
    case class Decode[R, E, A, B](decoder: Decoder[A], cb: A => Response[R, E, MessageHeader with MessageBody])             extends Response[R, E, Nothing]
    case class DecodeM[R, E, A, B](decoder: Decoder[A], cb: A => ZIO[R, E, Response[R, E, MessageHeader with MessageBody]]) extends Response[R, E, Nothing]
    case class JHttpResponse(jRes: JHttpResponse)                                                                           extends Response[Any, Nothing, MessageHeader]
    case class Complete[R, E](head: Head, buffer: ByteBuf)                                                                  extends Response[Any, Nothing, MessageHeader with MessageBody]
    case class Queued[R, E](head: Head, queue: Queue[R, E, ByteBuf])                                                        extends Response[R, E, MessageHeader with MessageBody]
    case class JFullHttpResponse(jRes: JFullHttpResponse)                                                                   extends Response[Any, Nothing, MessageHeader with MessageBody]
    case class Default[R, E](head: Head, content: Content[R, E])                                                            extends Response[R, E, MessageHeader with MessageBody]

    def apply[R, E](status: Status = Status.Ok, headers: List[Header] = Nil, content: Content[R, E] = Content.empty): Response[R, E, MessageHeader with MessageBody] =
      Default(Head(status, headers), content)

    def decode[R, E, A](d: Decoder[A])(cb: A => Response[R, E, MessageHeader with MessageBody]): Response[R, E, Nothing]             = Decode(d, cb)
    def decodeM[R, E, A](d: Decoder[A])(cb: A => ZIO[R, E, Response[R, E, MessageHeader with MessageBody]]): Response[R, E, Nothing] = DecodeM(d, cb)
  }

  sealed trait Request[-R, +E, +A] {
    def head: Request.Head = ???
  }

  object Request {
    case class Head(method: Method, url: Url, headers: List[Header]) extends Request[Any, Nothing, MessageHeader]
    case class JHttpRequest(jReq: JHttpRequest)                      extends Request[Any, Nothing, MessageHeader]
    case class Complete[R, E](head: Head, buffer: ByteBuf)           extends Request[R, E, MessageHeader with MessageBody]
    case class Queued[R, E](head: Head, queue: Queue[R, E, ByteBuf]) extends Request[R, E, MessageHeader with MessageBody]
    case class JHttpFullRequest(jReq: JFullHttpRequest)              extends Request[Any, Nothing, MessageHeader with MessageBody]
  }

  // Basic Example
  val a0 = HttpApp.collect { case req =>
    Response(
      status = Status.Ok,
      content = Content.fromString("Tushar"),
    )
  }

  // Complete Response
  val a1 = HttpApp.collect { case req =>
    Response.decode(Decoder.asComplete) { bytes =>
      val r = Request.Complete(req.head, bytes)
      Response(
        status = Status.Ok,
        content = Content.fromByteBuf(bytes),
      )
    }
  }

  def complete[R, E](pf: PartialFunction[Request[Any, Nothing, MessageHeader with MessageBody], Response[R, E, MessageHeader with MessageBody]]) = HttpApp.collect { case req =>
    Response.decode(Decoder.asComplete) { bytes =>
      val r = Request.Complete(req.head, bytes)
      pf(r)
    }
  }
}

// Issues
// Performance
