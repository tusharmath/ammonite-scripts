object Response_0 {

  /**
   * Create a response domain that can be used on both — the clients and the server
   */

  type Header
  type SocketApp[-R, +E]

  type ByteBuf
  type Queue[-R, +E, +A]
  type ZIO[-R, +E, +A]

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

  // Request
  sealed trait Request[-R, +E]
  object Request {
    case class Basic(method: Method, url: Url, headers: List[Header])                                          extends Request[Any, Nothing]
    case class Streaming[R, E](method: Method, url: Url, headers: List[Header], content: Queue[R, E, ByteBuf]) extends Request[R, E]
  }

  // Response
  sealed trait Response[-R, +E]

  object Response {
    sealed trait Writable[-R, +E] extends Response[R, E]
    object Writable {
      final case class Streaming[R, E](status: Status = Status.Ok, headers: List[Header] = Nil, content: Queue[R, E, ByteBuf]) extends Writable[R, E]
      final case class Complete(status: Status = Status.Ok, headers: List[Header] = Nil, content: ByteBuf)               extends Writable[Any, Nothing]
    }

    final case class Socket[R, E](socket: SocketApp[R, E])                                  extends Response[R, E]
    final case class Decode[R, E, A](decoder: Decoder[A], cb: A => Writable[R, E])             extends Response[R, E]
    final case class DecodeM[R, E, A](decoder: Decoder[A], cb: A => ZIO[R, E, Writable[R, E]]) extends Response[R, E]

    def decode[R, E, A](decoder: Decoder[A])(cb: A => Writable[R, E]): Response[R, E] = Decode(decoder, cb)
  }
  // Decoder

  sealed trait Decoder[+A]
  object Decoder {
    case object AsQueue    extends Decoder[Queue[Any, Nothing, ByteBuf]]
    case object AsComplete extends Decoder[ByteBuf]
    def asQueue[R, E]: Decoder[Queue[R, E, ByteBuf]] = AsQueue
    def asComplete: Decoder[ByteBuf]     = AsComplete
  }

  // Example
  def server[R, E](req: Request.Basic)(cb: Request.Streaming[R, E] => Response.Writable[R, E]): Response[R, E] =
    Response.decode(Decoder.asQueue)(q => cb(Request.Streaming(req.method, req.url, req.headers, q)))

   

}
