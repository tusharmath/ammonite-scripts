package response

object Response_1 {

  /**
   * Create a response domain that can be used on both — the clients and the server
   */

  type Header
  type SocketApp[-R, +E]

  type ByteBuf
  type Queue[-R, +E, +A]
  type ZIO[-R, +E, +A]
  type UIO[A] = ZIO[Any, Nothing, A]

  type Url
  // Type of content
  type Complete
  type Chunked

  def app(req: Request.Complete[Any, Nothing]): UIO[Response.Complete[Any, Nothing]] = ???

  // Method
  sealed trait Method

  // Status
  sealed trait Status

  sealed trait Message[-R, +E, +A] {
    def map[B](ab: A => B): Message[R, E, B]                                           = ???
    def flatMap[R1 <: R, E1 >: E, B](afb: A => Message[R1, E1, B]): Message[R1, E1, B] = ???

    def asCompleteRequest[R1 <: R, E1 >: E](implicit ev: A <:< Request[R1, E1, _]): Message[R1, E1, Request.Complete[R1, E1]]                      = ???
    def asChunkedRequest[R1 <: R, E1 >: E](implicit ev: A <:< Request[R1, E1, Queue[R1, E1, ByteBuf]]): Message[R1, E1, Request.Streaming[R1, E1]] = ???
  }

  sealed trait Request[-R, +E, +A]

  sealed trait Response[-R, +E, +A]

  object Method {
    case object Get  extends Method
    case object Post extends Method
  }

  object Status {
    case object Ok       extends Status
    case object NotFound extends Status
  }

  object Message {
    def fromEffect[R, E, A](z: ZIO[R, E, A]): Message[R, E, A] = FromEffect(z)

    def fromResponse[R, E, A](res: Response[R, E, A]): Message[R, E, Response[R, E, A]] = FromResponse(res)

    case class FromRequest[R, E, A](req: Request[R, E, A]) extends Message[R, E, Request[R, E, A]]

    case class FromResponse[R, E, A](res: Response[R, E, A]) extends Message[R, E, Response[R, E, A]]

    case class FromEffect[R, E, A](zio: ZIO[R, E, A]) extends Message[R, E, A]
  }

  object Request {
    type Complete[-R, +E]  = Request[R, E, Chunked]
    type Streaming[-R, +E] = Request[R, E, Queue[R, E, ByteBuf]]

    final case class Http[R, E, A](method: Method, url: Url, headers: List[Header] = Nil, content: A) extends Request[R, E, A]
  }

  object Response {
    type Complete[-R, +E]  = Request[R, E, ByteBuf]
    type Streaming[-R, +E] = Request[R, E, Queue[R, E, ByteBuf]]

    final case class Http[R, E, A](status: Status = Status.Ok, headers: List[Header] = Nil, content: A) extends Response[R, E, A]
    final case class Socket[R, E](app: SocketApp[R, E])                                                 extends Response[R, E, Nothing]
  }

  // def server[R, E, A, B](msg: Message[Any, Nothing, Request[Any, Nothing, A]]): Message[R, E, Response[R, E, B]] = for {
  //   req <- msg.asCompleteRequest
  //   res <- Message.fromEffect(app(req))
  //   _   <- Message.fromResponse(res)
  // } yield res

}
