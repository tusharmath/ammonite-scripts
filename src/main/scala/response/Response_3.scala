package response

object Response_3 {

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
  type MessageHeader
  type MessageBody

  // Method
  sealed trait Method

  // Status
  sealed trait Status

  sealed trait Request[+C, +A] { self =>

    def url: Url = Request.url(self)
  }

  object Method {
    case object Get  extends Method
    case object Post extends Method
  }

  object Status {
    case object Ok       extends Status
    case object NotFound extends Status
  }

  object Request {
    def url[C, A](req: Request[C, A]): Url =
      req match {
        case Head0(method, url, headers)  => ???
        case Full(messageHeader, content) => ???
        case FromJRequest(jReq)           => ???
      }

    final case class Head0(method: Method, override val url: Url, headers: List[Header]) extends Request[Nothing, MessageHeader]

    final case class Full[C](messageHeader: Head0, content: C) extends Request[C, MessageHeader with MessageBody]

    final case class FromJRequest(jReq: JHttpRequest) extends Request[Nothing, MessageHeader]
  }
}
