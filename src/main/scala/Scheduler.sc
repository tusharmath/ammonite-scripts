import java.time.Duration
import java.time.OffsetTime
sealed trait Decision[+A]

sealed trait Task[+A] {
  def map[B](ab: A => B): Task[B]
}

object Decision {
  case class Complete[A](value: A) extends Decision[A]
  case class More[A](value: A)     extends Decision[A]
}

case class Schedule[-A, +B](run: (OffsetTime, A) => Task[Decision[B]]) {
  // def map[C](bc: B => C): Schedule[A, C] = Schedule((time, a) => bc(run(time, a)))
}

object Schedule {
  def fixed(duration: Duration): Schedule[Any, Nothing] = ??? ///Schedule()
}
