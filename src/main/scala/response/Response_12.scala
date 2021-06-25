package response

import zio.{Chunk, ZIO}

object Response_12 {
  type Header
  type SocketServer[-R, +E]
  type ByteBuf
  type JHttpRequest
  type JFullHttpRequest
  type JHttpResponse
  type JFullHttpResponse
  type Url
  type UHttp[-A, +B] = Http[Any, Nothing, A, B]

// Method
  sealed trait Method

// Status
  sealed trait Status

  sealed trait Content[-R, +E, +A] { self => }

  sealed trait Event[+A]

/// --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

  sealed trait Operation[+A]

  sealed trait Request

  sealed trait Response {
    def toBytes: Chunk[Byte]
  }

  trait Http[-R, +E, -A, +B] {
    def execute(a: A): ZIO[R, E, B]
    def map[C](bc: B => C): Http[R, E, A, C]
    def contramap[X](xa: X => A): Http[R, E, X, B]
    def collect[X](pf: PartialFunction[X, A]): Http[R, E, X, B]
    def collectM[X]: Http.MkHttpCollectM[R, E, X, A, B]
  }

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

  object Content {
    def fromByteBuf(data: ByteBuf): Content[Any, Nothing, ByteBuf] = CompleteContent(data)

    def empty: Content[Any, Nothing, Nothing] = EmptyContent

    final case class CompleteContent(bytes: ByteBuf) extends Content[Any, Nothing, ByteBuf]

    case object EmptyContent extends Content[Any, Nothing, Nothing]
  }

  object Http {
    def collect[A]: MkHttp[A]                 = new MkHttp(())
    def collectM[A]: MkHttpM[A]               = new MkHttpM(())
    def route[A]: MkRoute[A]                  = new MkRoute(())
    def identity[A]: Http[Any, Nothing, A, A] = ???

    final class MkHttp[A](val unit: Unit) extends AnyVal {
      def apply[B](pf: PartialFunction[A, B]): Http[Any, Nothing, A, B] = ???
    }

    final class MkHttpM[A](val unit: Unit) extends AnyVal {
      def apply[R, E, B](pf: PartialFunction[A, ZIO[R, Option[E], B]]): Http[R, E, A, B] = ???
    }

    final class MkHttpCollectM[-R, +E, X, -A, +B](val unit: Unit) extends AnyVal {
      def apply[R1 <: R, E1 >: E](pf: PartialFunction[X, ZIO[R1, Option[E1], A]]): Http[R1, E1, X, B] = ???
    }

    final class MkRoute[A](val unit: Unit) extends AnyVal {
      def apply[R, E, B](pf: PartialFunction[A, Http[R, Any, E, B]]): Http[R, E, A, B] = ???
    }
  }

  object Event {
    case class Read[A](msg: Chunk[A]) extends Event[A]
    case object Complete              extends Event[Nothing]
  }

  object Operation {
    def write[A](a: Chunk[A]): Operation[A] = ???

    def empty: Operation[Nothing] = ???

    case class Write[A](msg: Chunk[A]) extends Operation[A]

    case object Empty extends Operation[Nothing]

    case object Read extends Operation[Nothing]
  }

  object Request {
    def fromBytes(chunk: Chunk[Byte]): Request = ???
  }

  object Response {
    def apply[R, E, A](status: Status = Status.Ok, headers: List[Header] = Nil, content: Content[R, E, A] = Content.empty): Response =
      ???
  }

// val echo = Http.identity

  object Example {
    import Event._

    val app = Http.collect[Request]({ case req => Response() })

    val b = app
      .map(res => Operation.write(res.toBytes))
      .collectM[Event[Byte]]({
        case Read(bytes) => ZIO.succeed(Request.fromBytes(bytes))
        case Complete    => ZIO.fail(None)
      })
  }
}
