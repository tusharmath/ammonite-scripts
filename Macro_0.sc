import scala.annotation.compileTimeOnly

import $ivy.`dev.zio::zio:1.0.3`

import scala.annotation.{StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.runtime.universe._

import zio._
object Service {
  import scala.reflect.macros.blackbox.Context

  object debug {
    def make[T](x: => T): T = macro impl
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

  debug.make {
    println("HELLO!")
  }
  object Foo {
    final case class Live() {
      def foo: Int = ???
      def bar: String = ???
      private def zoo: Byte = ???
    }
  }

  def run() = {

    val q"object $foo { final case class Live() {..$body} }" = q"""
    object Foo {
      final case class Live() {
        def foo: Int = ???
        def bar: String = ???
        private def zoo: Byte = ???
      }
    }
    """

    // println(body)
  }
}

Service.run()
