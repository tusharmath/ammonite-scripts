import $ivy.`dev.zio::zio:1.0.8`
import $ivy.`dev.zio::zio-streams:1.0.8`

import zio._
import zio.stream._

sealed trait Event[+A]
object Event {
  case class Read[A](msg: A) extends Event[A]
  case object Complete       extends Event[Nothing]
}

sealed trait Operation[+A]
object Operation {
  case object Empty            extends Operation[Nothing]
  case object Read             extends Operation[Nothing]
  case class Write[A](data: A) extends Operation[A]
}

case class Channel[-R, +E, -A, +B](run: Event[A] => ZIO[R, E, Operation[B]])

trait Request
trait Response

def ch: Channel[Any, Nothing, Request, Response] = ???
 
def channelRead(msg: Request) = ch.run(Event.Read(msg))
def channelReadComplete(msg: Request) = ch.run(Event.Read(msg))
