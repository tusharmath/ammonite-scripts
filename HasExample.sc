// scala 3.0.0-RC1
// ammonite 2.2.0

println("Ok!")

trait Has[A] { self =>
  def ++[B <: Has[_]](other: B): B with Has[A] = ???
}
final case class A()
final case class B()
final case class C()

def a: Has[A] = ???
def b: Has[B] = ???
def c: Has[C] = ???

def abc = a ++ b ++ c

trait Foo[A]

  def foo(cb: A => Unit): Unit

  def bar(a: A) = ???

  def baz = {

    foo { a => bar(a) }
  }


println("ok!")
