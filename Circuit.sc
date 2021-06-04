// scala 2.13.3
// ammonite 2.2.0

import $ivy.`dev.zio::zio:1.0.3`
import zio._

sealed trait Anomaly 
object Anomaly {
  x
}

sealed trait Fuse

object Fuse {
  case object Open                                      extends Fuse
  case object Close                                     extends Fuse
  case class Count(min: Long, max: Long, current: Long) extends Fuse

  def max(max: Long): Fuse = Count(Long.MinValue, max, 0)
}

object Example {
  // Circuit opens when a limit is achieved.

  // if (fuse.isClosed) {
  //   fuse.inc()
  //   acceptConnection
  //   fuse.dec()
  // }

  // if (fuse.isClosed) {
  //   try {
  //     acceptConnection
  //   } catch {
  //     case _ =>
  //       subscriptionCounter.inc()
  //       delay(100, subscriptionCounter.dec())
  //   }

  // }

}

println("Yo!")
