// FORGET PERFORMANCE FOR A MINUTE

type HttpResult[-R, +E, +A]
type JChannelHandlerContext

// Netty Channel Handler
trait JChannelHandler[A] {
  def channelRead(ctx: JChannelHandlerContext, msg: A): Any = ???
  def channelReadComplete(ctx: JChannelHandlerContext): Any = ???
}

case class Channel[R, E, A, B](execute: A => HttpResult[R, E, B])

object Channel {

}
