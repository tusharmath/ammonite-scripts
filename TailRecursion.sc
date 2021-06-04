// scala 2.13.3
// ammonite 2.2.0

import scala.annotation.tailrec

sealed trait Trampoline[A] { self =>
  def flatMap[B](afb: A => Trampoline[B]): Trampoline[B] = Trampoline.FlatMap(self, afb)
  def map[B](ab: A => B): Trampoline[B]                  = self.flatMap(a => Trampoline.done(ab(a)))  
}

object Trampoline {
  case class Done[A](a: A)                                           extends Trampoline[A]
  case class FlatMap[A, B](fa: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]


  def unit: Trampoline[Unit]                      = Trampoline.done(())
  def done[A](a: A): Trampoline[A]                = Done(a)
  def more[A](a: => Trampoline[A]): Trampoline[A] = Trampoline.unit.flatMap(_ => a)


  @tailrec
  def eval[A](a: Trampoline[A]): A =
    a match {
      case Done(a)        => a
      case FlatMap(fa, f) =>
        eval(
          fa match {
            case Done(a)          => f(a)
            case FlatMap(fa0, f0) =>
              println("FM")
              fa0.flatMap(a0 => f0(a0).flatMap(f))
          },
        )
    }
}

def sum(i: Long): Trampoline[Long] = i match {
  case 0 => Trampoline.done(0)
  case j => Trampoline.more(sum(i - 1).flatMap(s => Trampoline.done(j + s)))
}

println("Start!")
println(
  Trampoline.eval(sum(5L)),
)
