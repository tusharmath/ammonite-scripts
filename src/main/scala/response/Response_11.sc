import $ivy.`dev.zio::zio:1.0.8`
import $ivy.`dev.zio::zio-streams:1.0.8`

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
type JChannelHandlerContext
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
  case object Continue extends Status
}

// Path
sealed trait Path { self =>
  def asString: String
  def /(name: String): Path     = ???
  def toList: List[String]
  override def toString: String = this.asString
}

object Path {
  def apply(): Path                               = ???
  def apply(string: String): Path                 = ???
  def apply(seqString: String*): Path             = ???
  def apply(list: List[String]): Path             = ???
  def unapplySeq(arg: Path): Option[List[String]] = ???
  def empty: Path                                 = ???
}

case class /(path: Path, name: String) extends Path {
  override lazy val asString: String = s"${path.asString}/${name}"
  override def toList: List[String]  = path.toList ::: List(name)
}

case object Root extends Path {
  override lazy val asString: String = ""
  override def toList: List[String]  = Nil
}

/// --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

type Http[-R, +E, -A, +B]
type Route = (Method, Path)

type Buffered
type Complete
type Opaque

final case class InvalidAccess(property: String, obj: Any) extends Throwable {
  override def getMessage(): String = s"""The property "$property" is unavailable on: $obj"""
}

/**
 * Extract `Content` from `Request` and `Response`
 */
sealed trait HasContent[-A] {
  type Out >: A

  def content[R, E, A1 <: Out](request: Request[R, E, A1]): Content[R, E, Out]   =
    request match {
      case Request.Default(_, _, _, dContent) => dContent
      case _                                  => throw InvalidAccess("content", request)
    }
  def content[R, E, A1 <: Out](response: Response[R, E, A1]): Content[R, E, Out] =
    response match {
      case Response.Default(_, _, dContent) => dContent
      case _                                => throw InvalidAccess("content", response)
    }
  def status[R, E, A1 <: A](response: Response[R, E, A1]): Status                = response match {
    case Response.Default(status, _, _) => status
    case _                              => throw InvalidAccess("status", response)
  }
  def headers[R, E, A1 <: A](response: Response[R, E, A1]): List[Header]         = response match {
    case Response.Default(_, headers, _) => headers
    case _                               => throw InvalidAccess("headers", response)
  }
}

object HasContent {
  implicit case object HasNothing  extends HasContent[Opaque]   {
    override type Out = Any
  }
  implicit case object HasBuffered extends HasContent[Buffered] {
    override type Out = Buffered
  }
  implicit case object HasComplete extends HasContent[Complete] {
    override type Out = Complete
  }
}

/**
 * Extracts data from `Content`
 */
sealed trait HasData[-A] {
  type Out[-R, +E]
  def data[R, E, A1 <: A](content: Content[R, E, A1]): Out[R, E]
}

object HasData {
  import Content._
  implicit case object Complete extends HasData[Complete] {
    override type Out[-R, +E] = ByteBuf
    override def data[R, E, A1 <: Complete](content: Content[R, E, A1]): Out[R, E] = content match {
      case CompleteContent(bytes) => bytes
      case _                      => throw new Error("Data is Unavailable")
    }
  }
  implicit case object Buffered extends HasData[Buffered] {
    override type Out[-R, +E] = ZStream[R, E, ByteBuf]
    override def data[R, E, A1 <: Buffered](content: Content[R, E, A1]): Out[R, E] = content match {
      case BufferedContent(source) => source
      case _                       => throw new Error("Data is Unavailable")
    }
  }
}

sealed trait Content[-R, +E, +A] { self =>
  def data[A1 >: A](implicit ev: HasData[A1]): ev.Out[R, E] = ev.data(self)
}

object Content {
  final case class CompleteContent(bytes: ByteBuf)                       extends Content[Any, Nothing, Complete]
  final case class BufferedContent[R, E](source: ZStream[R, E, ByteBuf]) extends Content[R, E, Buffered]
  case object EmptyContent                                               extends Content[Any, Nothing, Opaque]

  def fromByteBuf(data: ByteBuf): Content[Any, Nothing, Complete]             = CompleteContent(data)
  def fromStream[R, E](data: ZStream[R, E, ByteBuf]): Content[R, E, Buffered] = BufferedContent(data)
  def empty: Content[Any, Nothing, Opaque]                                    = EmptyContent
}

sealed trait Request[-R, +E, +A] { self =>
  def method: Method
  def url: Url
  def headers: List[Header]

  def widen[A1](implicit ev: A <:< A1): Request[R, E, A1] = ev.liftCo(self)

  def content(implicit ev: HasContent[A]): Content[R, E, ev.Out] = ev.content(self)

  def copy[R1, E1, A1](method: Method = self.method, url: Url = self.url, headers: List[Header] = self.headers, content: Content[R1, E1, A1]): Request[R1, E1, A1] =
    Request.Default(method, url, headers, content)
}

object Request {
  final case class Default[R, E, A](method: Method, url: Url, headers: List[Header], dContent: Content[R, E, A]) extends Request[R, E, A]
  final case class FromJHttpRequest(jReq: JHttpRequest)                                                          extends Request[Any, Nothing, Nothing] {
    override def method: Method        = ???
    override def url: Url              = ???
    override def headers: List[Header] = ???
  }
  def apply[R, E, A](method: Method, url: Url, headers: List[Header], content: Content[R, E, A]): Request[R, E, A] = Default[R, E, A](method, url, headers, content)
}

sealed trait Response[-R, +E, +A] { self =>
  def status(implicit ev: HasContent[A]): Status                 = ev.status(self)
  def headers(implicit ev: HasContent[A]): List[Header]          = ev.headers(self)
  def content(implicit ev: HasContent[A]): Content[R, E, ev.Out] = ev.content(self)
}

object Response {
  final case class Default[R, E, A](dStatus: Status, dHeaders: List[Header], dContent: Content[R, E, A]) extends Response[R, E, A]
  final case class Socket(server: SocketServer[Any, Nothing])                                            extends Response[Any, Nothing, Opaque]

  def apply[R, E, A](status: Status = Status.Ok, headers: List[Header] = Nil, content: Content[R, E, A] = Content.empty): Response[R, E, A] =
    Default(status, headers, content)
}

sealed trait Parser[-A, +B] {}

sealed trait Endpoint[-R, +E] {
  def ++[R1 <: R, E1 >: E](other: Endpoint[R1, E1]): Endpoint[R1, E1] = ???
}

object Endpoint {
  // def define[R, E, B: HasContent](route: Route)(http: Request[Nothing] => ZIO[R, Option[E], Response[B]]): Endpoint[R, E] = ???
  // def define[R, E](route: Route)(http: Request => ZIO[R, Option[E], Response]): Endpoint[R, E] = ???
}
