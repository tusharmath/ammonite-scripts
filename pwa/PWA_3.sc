// scala 2.13.3
// ammonite 2.2.0

import $ivy.`org.typelevel::cats-free:2.4.2`
import $plugin.$ivy.`org.typelevel:kind-projector_2.13.2:0.11.0`

import cats._
import cats.implicits._

type ~>[-A, +B] = PartialFunction[A, B]

object FreeCommandOps {
  sealed trait FreeCommand[F[_], +A]

  object FreeCommand {
    case class Combine[F[_], A](a: FreeCommand[F, A], b: FreeCommand[F, A]) extends FreeCommand[F, A]
    case class Empty[F[_]]()                                                extends FreeCommand[F, Nothing]
    case class FMap[F[_], A, B](fa: FreeCommand[F, A], f: A => B)           extends FreeCommand[F, B]
    case class Pure[F[_], A](fa: F[A])                                      extends FreeCommand[F, A]

    def lift[F[_], A](fa: F[A]): FreeCommand[F, A] = Pure(fa)
    def empty[F[_]]: FreeCommand[F, Nothing]       = Empty[F]()
  }

  implicit def functor[F[_]]: Functor[FreeCommand[F, ?]] = new Functor[FreeCommand[F, *]] {
    override def map[A, B](fa: FreeCommand[F, A])(f: A => B): FreeCommand[F, B] = FreeCommand.FMap(fa, f)
  }

  implicit def monoid[F[_], A]: Monoid[FreeCommand[F, A]] = new Monoid[FreeCommand[F, A]] {
    override def combine(x: FreeCommand[F, A], y: FreeCommand[F, A]): FreeCommand[F, A] = FreeCommand.Combine[F, A](x, y)
    override def empty: FreeCommand[F, A]                                               = FreeCommand.Empty()
  }
}

object ChannelOps {
  sealed trait Channel[S, -A, +B] extends Product with Serializable
  object Channel {
    private case class Initial[S](s: S)                                                  extends Channel[S, Any, Nothing]
    private case class Update[S, A](fun: ((S, A)) ~> S)                                  extends Channel[S, A, Nothing]
    private case class Command[S, A, B](fun: ((S, A)) ~> B)                              extends Channel[S, A, B]
    private case class Combine[S, A, B](self: Channel[S, A, B], other: Channel[S, A, B]) extends Channel[S, A, B]

    def init[S](s: S): Channel[S, Any, Nothing]                              = Initial(s)
    def update[S, A](pf: PartialFunction[(S, A), S]): Channel[S, A, Nothing] = Update[S, A](pf)
    def command[S, A]: MkCommand[S, A]                                       = MkCommand[S, A](())

    def execute[S, A, B](ch: Channel[S, A, B], sa: (S, A)): Eval[(S, B)] = ???

    final case class MkCommand[S, A](unit: Unit) extends AnyVal {
      def apply[B](pf: PartialFunction[(S, A), B]): Channel[S, A, B] = Command[S, A, B](pf)
    }

    implicit def semigroup[S, A, B] = new Semigroup[Channel[S, A, B]] {
      override def combine(x: Channel[S, A, B], y: Channel[S, A, B]): Channel[S, A, B] = Channel.Combine(x, y)
    }
  }
}

object ActionOps {

  /**
   * Input to the Channel
   */
  sealed trait Action extends Product with Serializable

  object Action {
    case object Unit                            extends Action
    case class ConnectionOpen(address: String)  extends Action
    case class ConnectionClose(address: String) extends Action
    case class Random(i: Int)                   extends Action
    case object Inc                             extends Action
    case object Dec                             extends Action
    case object Mul                             extends Action

    def inc: Action                              = Inc
    def dec: Action                              = Dec
    def mul: Action                              = Mul
    def random(i: Int): Action                   = Random(i)
    def connectionOpen(address: String): Action  = ConnectionOpen(address)
    def connectionClose(address: String): Action = ConnectionClose(address)
    def unit: Action                             = Unit
  }
}

object CommandOps {
  import FreeCommandOps._

  /**
   * Output from the channel.
   */
  sealed trait CommandA[+A] extends Product with Serializable

  type Command[A] = FreeCommand[CommandA, A]
  object Command {
    private case class Subscribe(topic: String)   extends CommandA[Unit]
    private case class Unsubscribe(topic: String) extends CommandA[Unit]
    private case object RandomInt                 extends CommandA[Int]

    def random: Command[Int]                      = FreeCommand.lift(RandomInt)
    def subscribe(topic: String): Command[Unit]   = FreeCommand.lift(Subscribe(topic))
    def unsubscribe(topic: String): Command[Unit] = FreeCommand.lift(Unsubscribe(topic))
    def empty: Command[Nothing]                   = FreeCommand.empty
  }
}

object Example {
  import ChannelOps._
  import CommandOps._
  import ActionOps._

  val ch0: Channel[Int, Action, Command[Action]] = Channel.command[Int, Action] {
    case s -> Action.Inc => Command.random.map(Action.random) |+| Command.subscribe("TUSHAR").as(Action.unit)
    case s -> Action.Dec => Command.unsubscribe("TUSHAR").as(Action.ConnectionClose("???"))
  }

  val ch1: Channel[Int, Action, Nothing] = Channel.update[Int, Action] {
    case s -> Action.Inc => s + 1
    case s -> Action.Dec => s - 1
  }

  val ch2: Channel[Int, Action, Nothing] = Channel.update[Int, Action] {
    case s -> Action.Mul => s * s
    case s -> _          => s * s
  }

  val ch3 = Channel.init(100)

  val ch = ch0 |+| ch1 |+| ch2 |+| ch3

  // Channel.execute(out, (100, "INC"))
}
