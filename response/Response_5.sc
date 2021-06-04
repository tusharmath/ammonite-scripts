// scala 2.13.6
// Ammonite 2.3.8

import $ivy.`dev.zio::zio:1.0.8`
object Response_5 {
  import zio._
  
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

  type CompleteBody
  type BufferedBody
  type MessageHeader

  sealed trait HasHeader[-A] {
    def status[A1 <: A](response: Response[A1]): Status
    def headers[A1 <: A](response: Response[A1]): List[Header]
  }

  implicit object HasHeader extends HasHeader[MessageHeader] {
    import Response._
    override def status[A1 <: MessageHeader](response: Response[A1]): Status =
      response match {
        case Default(status, _, _) => status
        case _                     => throw new Error(s"status unavailable on response: ${response}")
      }

    override def headers[A1 <: MessageHeader](response: Response[A1]): List[Header] =
      response match {
        case Default(_, header, _) => header
        case _                     => throw new Error(s"header unavailable on response: ${response}")
      }
  }

  sealed trait HasContent[-A]
  object HasContent {
    implicit case object AsQueue    extends HasContent[BufferedBody]
    implicit case object AsComplete extends HasContent[CompleteBody]
  }

  sealed trait Decoder[-A, B] {
    def content[A1 <: A](response: Response[A1]): B
    def content[A1 <: A](request: Request[A1]): B
  }

  object Decoder {
    implicit case object AsQueue extends Decoder[BufferedBody, Queue[ByteBuf]] {
      override def content[A1 <: BufferedBody](request: Request[A1]): Queue[ByteBuf] =
        request.asInstanceOf[Request.Default[_]].content.asInstanceOf[Content.Data[BufferedBody, Queue[ByteBuf]]].data

      override def content[A1 <: BufferedBody](response: Response[A1]): Queue[ByteBuf] =
        response.asInstanceOf[Response.Default[_]].content.asInstanceOf[Content.Data[BufferedBody, Queue[ByteBuf]]].data
    }

    implicit case object AsComplete extends Decoder[CompleteBody, ByteBuf] {
      override def content[A1 <: CompleteBody](request: Request[A1]): ByteBuf =
        request.asInstanceOf[Request.Default[_]].content.asInstanceOf[Content.Data[CompleteBody, ByteBuf]].data

      override def content[A1 <: CompleteBody](response: Response[A1]): ByteBuf =
        response.asInstanceOf[Response.Default[_]].content.asInstanceOf[Content.Data[CompleteBody, ByteBuf]].data
    }
  }

  sealed trait Content[A]

  object Content {
    final case class Data[A, B](d: Decoder[A, B], data: B) extends Content[A]
    case object Empty                                      extends Content[Nothing]

    def data[A, B](data: B)(implicit ev: Decoder[A, B]): Content[A] = Data(ev, data)
    def empty: Content[Nothing]                                     = Empty
  }

  sealed trait Request[A] { self =>
    def method: Method
    def url: Url
    def headers: List[Header]

    def content[B](implicit d: Decoder[A, B]): B = d.content(self)
  }

  object Request {
    final case class Default[A](method: Method, url: Url, headers: List[Header], content: Content[A]) extends Request[A]
    final case class FromJHttpRequest(jReq: JHttpRequest)                                             extends Request[Nothing] {
      def method: Method        = ???
      def url: Url              = ???
      def headers: List[Header] = ???
    }

    def apply[A](method: Method, url: Url, headers: List[Header], content: Content[A]): Request[A] = Default(method, url, headers, content)
  }

  sealed trait Response[A] { self =>
    def status(implicit h: HasHeader[A]): Status        = h.status(self)
    def headers(implicit h: HasHeader[A]): List[Header] = h.headers(self)
    def content[X](implicit d: Decoder[A, X]): X        = d.content(self)
  }

  object Response {
    final case class Default[A](status: Status, headers: List[Header], content: Content[A]) extends Response[A with MessageHeader]
    final case class Socket(server: SocketServer[Any, Nothing])                             extends Response[Nothing]
    final case class Decode[A, X, B](d: Decoder[A, X], cb: X => Response[B])                extends Response[Nothing]
    def decodeComplete[B: HasContent](cb: ByteBuf => Response[B])(implicit d: Decoder[CompleteBody, ByteBuf])               = Decode(d, cb)
    def decodeBuffered[B: HasContent](cb: Queue[ByteBuf] => Response[B])(implicit d: Decoder[BufferedBody, Queue[ByteBuf]]) = Decode(d, cb)
  }

  // def app(req: Request[CompleteBody]): Response[CompleteBody with MessageHeader] = Response.Head(Status.Ok, Nil, Content.data(req.content))

  def collectComplete[B: HasContent](req: Request[Nothing])(cb: Request[CompleteBody] => Response[B]) =
    Response.decodeComplete { bytes =>
      cb(Request.Default(req.method, req.url, req.headers, Content.data(bytes)))
    }

  // val a = Response.decodeA[CompleteBody].apply(x => Response.Head(Status.Ok, Nil, Content.Full(x)))
  val a = Response.decodeComplete(x => Response.Default(Status.Ok, Nil, Content.data(x)))

  // def program(req: Request[Nothing]) = {
  //   collectComplete(req)(reqWithBody => app(reqWithBody))
  // }

}
