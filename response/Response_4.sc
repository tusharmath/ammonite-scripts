import java.nio.Buffer
// scala 2.13.6

import $ivy.`dev.zio::zio:1.0.8`
object Response_4 {
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
  sealed trait MessageBody
  trait CompleteBody extends MessageBody
  trait BufferedBody extends MessageBody

  sealed trait Decoder[A]
  object Decoder {
    case object AsQueue    extends Decoder[Queue[ByteBuf]]
    case object AsComplete extends Decoder[ByteBuf]
    def asQueue: Decoder[Queue[ByteBuf]] = AsQueue
    def asComplete: Decoder[ByteBuf]     = AsComplete
  }

  sealed trait Content[A] {
    def widen[A1](implicit ev: A <:< A1): Content[A1] = ???
  }

  object Content {
    case class Buffered(queue: Queue[ByteBuf]) extends Content[BufferedBody]
    case class Full(bytes: ByteBuf)            extends Content[CompleteBody]
    case object Empty                          extends Content[Nothing]
  }

  sealed trait Request[A] { self =>
    def method: Method
    def url: Url
    def headers: List[Header]

    def completeContent(implicit ev: A =:= CompleteBody): ByteBuf        = ???
    def BufferedContent(implicit ev: A =:= BufferedBody): Queue[ByteBuf] = ???

    def widen[A1](implicit ev: A <:< A1): Request[A1] = self.asInstanceOf[Request[A1]]
  }

  object Request {
    case class Default[A](method: Method, url: Url, headers: List[Header], content: Content[A]) extends Request[A]

    case class FromJHttpRequest(jReq: JHttpRequest) extends Request[Nothing] {
      def method: Method        = ???
      def url: Url              = ???
      def headers: List[Header] = ???
    }
  }

  sealed trait Response[A]
  object Response {
    case class Head[A](status: Status, headers: List[Header], content: Content[A]) extends Response[A]
    case class Decode[A](d: Decoder[A], cb: A => Response[MessageBody])            extends Response[Nothing]

    def decode[A](d: Decoder[A])(cb: A => Response[MessageBody]): Response[Nothing] = Decode(d, cb)
  }

  def app(req: Request[CompleteBody]): Response[MessageBody] = {
    val bytes = req.completeContent
    Response.Head(Status.Ok, Nil, Content.Full(bytes).widen[MessageBody])
  }

  def collectComplete(req: Request[Nothing])(cb: Request[CompleteBody] => Response[MessageBody]) = {
    Response.decode(Decoder.asComplete) { bytes =>
      cb(Request.Default(req.method, req.url, req.headers, Content.Full(bytes)).widen)
    }
  }

  def program(req: Request[Nothing]) = {
    collectComplete(req)(reqWithBody => app(reqWithBody))
  }
}
