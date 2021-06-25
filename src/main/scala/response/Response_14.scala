package response

import zio.stream.UStream
import zio.ZIO

object Response_14 {

  type ByteBuf
  type Response[+A]
  type Request[+A]

  type AnyResponse       = Response[Any]
  type AnyRequest        = Request[Any]
  type UHttp[-A, +B]     = Http[Any, Nothing, A, B]
  type CompleteRequest   = Request[ByteBuf]
  type StreamingRequest  = Request[UStream[ByteBuf]]
  type CompleteResponse  = Response[ByteBuf]
  type StreamingResponse = Response[UStream[ByteBuf]]
  val a0 = Http.collect[CompleteRequest] { case req => StreamingResponse() }
  val a1 = Http.collect[StreamingRequest] { case req => CompleteResponse() }
//  val app = a0.process +++ a1.process

  sealed trait Widen[A] {
    type Out
  }

  trait Http[-R, +E, -A, +B] {
    def execute(a: A): ZIO[R, E, B]
    def map[C](bc: B => C): Http[R, E, A, C]
    def contramap[X](xa: X => A): Http[R, E, X, B]
    def collect[X](pf: PartialFunction[X, A]): Http[R, E, X, B]
    def +++[R1 <: R, E1 >: E, A1 <: A, B1 >: B](other: Http[R1, E1, A1, B1]): Http[R1, E1, A1, B1] = ???
  }

  object Http {
    def collect[A]: MkHttp[A]   = new MkHttp(())
    def collectM[A]: MkHttpM[A] = new MkHttpM(())
    def route[A]: MkRoute[A]    = new MkRoute(())

    final class MkHttp[A](val unit: Unit) extends AnyVal {
      def apply[B](pf: PartialFunction[A, B]): Http[Any, Nothing, A, B] = ???
    }

    final class MkHttpM[A](val unit: Unit) extends AnyVal {
      def apply[R, E, B](pf: PartialFunction[A, ZIO[R, Option[E], B]]): Http[R, E, A, B] = ???
    }

    final class MkRoute[A](val unit: Unit) extends AnyVal {
      def apply[R, E, B](pf: PartialFunction[A, Http[R, Any, E, B]]): Http[R, E, A, B] = ???
    }
  }

  implicit class HttpSyntax[-R, +E, -A, +B](http: Http[R, E, A, B]) {
    def process[A1 <: A, B1 >: B, A2, B2](implicit
      req: Widen.Aux[A1, A2],
      res: Widen.Aux[B1, B2],
    ): Http[R, E, A2, B2] =
      ???
  }

  object CompleteResponse {
    def apply(): CompleteResponse = ???
  }

  object StreamingResponse {
    def apply(): StreamingResponse = ???
  }

  object Widen {
    type Aux[A, B] = Widen[A] {
      type Out = B
    }

    implicit object WidenCompleteResponse  extends Widen[CompleteResponse]  {
      override type Out = AnyResponse
    }
    implicit object WidenStreamingResponse extends Widen[StreamingResponse] {
      override type Out = AnyResponse
    }

    implicit object WidenCompleteRequest  extends Widen[CompleteRequest]  {
      override type Out = AnyRequest
    }
    implicit object WidenStreamingRequest extends Widen[StreamingRequest] {
      override type Out = AnyRequest
    }
  }
}
