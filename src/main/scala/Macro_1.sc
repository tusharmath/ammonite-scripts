import scala.reflect.macros.whitebox.Context
import scala.annotation.compileTimeOnly

import $ivy.`dev.zio::zio:1.0.3`

import scala.annotation.{StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.runtime.universe._

import zio._

object debug {
  def doooo[T](x: => T): T = macro impl
  def impl(c: Context)(x: c.Tree) = {
    import c.universe._
    val q"..$stats" = x
    val loggedStats = stats.flatMap { stat =>
      val msg = "executing " + showCode(stat)
      List(q"println($msg)", stat)
    }
    q"..$loggedStats"
  }
}

// usage
object Test extends App {
  def faulty: Int = throw new Exception
  debug.doooo {
    val x = 1
    val y = x + faulty
    x + y
  }
}
