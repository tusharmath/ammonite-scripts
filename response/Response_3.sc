import $ivy.`dev.zio::zio:1.0.8`

object Response_3 {

  import zio._

  /**
   * Create a response domain that can be used on both — the clients and the server
   */

  type Header
  type SocketServer[-R, +E]

  type ByteBuf
  type Queue[-R, +E, +A]
  type JHttpRequest
  type JHttpResponse

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

  type MessageHeader
  type MessageBody

  sealed trait Request[+C, +A] { self =>

    def url: Url = Request.url(self)
  }
  object Request {
    final case class Head0(method: Method, override val url: Url, headers: List[Header]) extends Request[Nothing, MessageHeader]
    final case class Full[C](messageHeader: Head0, content: C)                           extends Request[C, MessageHeader with MessageBody]
    final case class FromJRequest(jReq: JHttpRequest)                                    extends Request[Nothing, MessageHeader]

    def url[C, A](req: Request[C, A]): Url =
      req match {
        case Head0(method, url, headers)  => ???
        case Full(messageHeader, content) => ???
        case FromJRequest(jReq)           => ???
      }
  }
}

// Issues
// Performance
