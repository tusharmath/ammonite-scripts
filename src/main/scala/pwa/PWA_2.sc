// scala 2.13.5
// ammonite 2.2.0

// PWA

import $ivy.`org.typelevel::cats-free:2.4.2`

import cats._
// import cats.syntax._
import cats.implicits._

sealed trait Channel[S, -A, +B] extends Product with Serializable

object Channel {
  private case class Initial[S](s: S)                                                  extends Channel[S, Any, Nothing]
  private case class Update[S, A](fun: ((S, A)) => S)                                  extends Channel[S, A, Nothing]
  private case class Command[S, A, B](fun: ((S, A)) => B)                              extends Channel[S, A, B]
  private case class Combine[S, A, B](self: Channel[S, A, B], other: Channel[S, A, B]) extends Channel[S, A, B]

  def init[S](s: S): Channel[S, Any, Nothing]                              = Initial(s)
  def update[S, A](pf: PartialFunction[(S, A), S]): Channel[S, A, Nothing] = Update[S, A](sa => if (pf.isDefinedAt(sa)) pf(sa) else sa._1)
  def command[S, A]                                                        = MkCommand[S, A](())

  def execute[S, A, B: Monoid](ch: Channel[S, A, B], sa: (S, A)): Eval[(S, B)] =
    ch match {
      case Update(fun)          => Eval.now(fun(sa), Monoid[B].empty)
      case Command(fun)         => Eval.now(sa._1, fun(sa))
      case Combine(self, other) =>
        Eval.defer {
          for {
            sa0 <- Channel.execute(self, sa)
            sa1 <- Channel.execute(other, (sa0._1, sa._2))
          } yield (sa1._1, Monoid[B].combine(sa0._2, sa1._2))
        }
    }

  implicit def channelSemigroup[S, A, B]: Semigroup[Channel[S, A, B]] = new Semigroup[Channel[S, A, B]] {
    override def combine(x: Channel[S, A, B], y: Channel[S, A, B]): Channel[S, A, B] = Channel.Combine(x, y)
  }

  final case class MkCommand[S, A](unit: Unit) extends AnyVal {
    def apply[B: Monoid](pf: PartialFunction[(S, A), B]): Channel[S, A, B] =
      Command[S, A, B](sa => if (pf.isDefinedAt(sa)) pf(sa) else Monoid[B].empty)
  }
}

object Example {
  sealed trait Action extends Product with Serializable
  object Action {
    case class ConnectionOpen(address: String)  extends Action
    case class ConnectionClose(address: String) extends Action

    case class Random(i: Int) extends Action
  }

  sealed trait Command[+A] extends Product with Serializable
  object Command {
    private case class Subscribe(topic: String)              extends Command[Unit]
    private case class Unsubscribe(topic: String)            extends Command[Unit]
    private case class Map[A, B](fa: Command[A], ab: A => B) extends Command[B]
    private case object RandomInt                            extends Command[Int]

    private case object Empty                                     extends Command[Nothing]
    private case class Combine[A](c1: Command[A], c2: Command[A]) extends Command[A]

    implicit def commandMonoid[A]: Monoid[Command[A]] = new Monoid[Command[A]] {
      def combine(x: Command[A], y: Command[A]): Command[A] = Combine(x, y)
      def empty: Command[A]                                 = Empty
    }

    implicit def commandFunctor[A]: Functor[Command] = new Functor[Command] {
      override def map[A, B](fa: Command[A])(f: A => B): Command[B] = Command.Map(fa, f)
    }

    def subscribe(topic: String): Command[Unit]   = Subscribe(topic)
    def unsubscribe(topic: String): Command[Unit] = Unsubscribe(topic)
    def empty: Command[Nothing]                   = Empty
    def random: Command[Int]                      = RandomInt
  }

  val up0 = Channel.update[Int, String] {
    case s -> "inc" => s + 1
    case s -> "dec" => s - 1
  }

  val up1 = Channel.update[Int, String] {
    case s -> "sqr" => s * s
    case s -> _     => s * s
  }

  val cm0 = Channel.command[Int, String] {
    case s -> "inc" => Command.random.map(i => Action.Random(i))
    case s -> "dec" => Command.unsubscribe("TUSHAR").as(Action.ConnectionClose(""))
  }

  val in0 = Channel.init(100)

  val out = cm0 |+| up0 |+| up1 |+| in0

  Channel.execute(out, (100, "INC"))
}
