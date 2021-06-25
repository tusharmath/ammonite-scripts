trait Observable[+A] { self =>
  def map[B](ab: A => B): Observable[B]                 = ???
  def flatMap[B](ab: A => Observable[B]): Observable[B] = Observable.FlatMap[A, B](self, ab)

  def subscribe[A1 >: A](o: Observer[A1]): Subscription = ???
}

object Observable {
  case class Succeed[A](a: A)                                        extends Observable[A]
  case class FlatMap[A, B](o: Observable[A], ab: A => Observable[B]) extends Observable[B]

}

sealed trait Observer[+A] {
  def next[A1 >: A](a: A1): Status
  def complete: Status
}

sealed trait Status

trait Subscription {
  def cancel: Status
}
