// scala 2.13.3
// ammonite 2.2.0

import $ivy.`org.typelevel::cats-free:2.4.2`

import cats.free.FreeApplicative
import cats.free.FreeApplicative.lift
import cats.implicits._

type Dom
type Response

type Command[A] = FreeApplicative[CommandA, A]

sealed trait Component[A, S] { self =>
  def &&&[A1](other: Component[A1, S])(implicit ev: A1 <:< A): Component[A1, S] =
    Component.Combine(self.asInstanceOf[Component[A1, S]], other)

  def init: S                         = ???
  def update(a: A, s: S): S           = ???
  def command(a: A, s: S): Command[A] = ???

  def install[T, A1, S1](c1: Component[A1, S1])(pf: PartialFunction[(S, A), (S1, A1)])(implicit ev: Component.Convertor[A, A1]) =
    self &&&
      Component.command[A, S] {
        case i if pf.isDefinedAt(i) =>
          val o = pf.apply(i)
          c1.command(o._2, o._1).map(ev.out)

      }
}

object Component {
  final case object Empty                                                  extends Component[Any, Nothing]
  final case class Initialize[S](s: S)                                     extends Component[Any, S]
  final case class Update[A, S](ss: PartialFunction[(S, A), S])            extends Component[A, S]
  final case class CCommand[A, S](ss: PartialFunction[(S, A), Command[A]]) extends Component[A, S]
  final case class View[S](ss: S => Dom)                                   extends Component[Any, S]
  final case class Combine[A, S](a: Component[A, S], b: Component[A, S])   extends Component[A, S]

  def empty: Component[Any, Nothing]                                          = Component.Empty
  def init[S](s: S): Component[Any, S]                                        = Initialize(s)
  def update[A, S](pf: PartialFunction[(S, A), S]): Component[A, S]           = Update(pf)
  def command[A, S](pf: PartialFunction[(S, A), Command[A]]): Component[A, S] = CCommand(pf)

  trait Convertor[A0, A1] {
    def out(a1: A1): A0
  }
}

sealed trait CommandA[+A]

object CommandA {
  case object Empty           extends CommandA[Nothing]
  case class Succeed[A](a: A) extends CommandA[A]
}

object Command extends TimerCommands with RandomCommands with HttpCommands with ComponentCommands {
  def empty: Command[Nothing]      = lift(CommandA.Empty)
  def succeed[A](a: A): Command[A] = lift(CommandA.Succeed(a))
}

trait ComponentCommands {
  case class Message[A](msg: A) extends CommandA[A]
  def msg[A](m: A): Command[A] = lift(Message(m))
}

trait TimerCommands {
  case class Delay(duration: Long) extends CommandA[Unit]
  def delay(duration: Long): Command[Unit] = lift(Delay(duration))
}

trait RandomCommands {
  case object GenerateRandom extends CommandA[Long]
  def randomLong: Command[Long] = lift(GenerateRandom)
  def randomInt: Command[Int]   = randomLong.map(_.toInt)
  def randomChar: Command[Char] = randomLong.map(i => (i.toInt % 26).toChar)
}

trait HttpCommands {
  case class Request(url: String) extends CommandA[Response]
  def request[A](url: String): Command[Response] = lift(Request(url))
}

// Example Usage

object Example {

  implicit val clockConvertor: Component.Convertor[CounterComponent.Msg, ClockComponent.Msg] = a => CounterComponent.Msg.Clock(a)

  object CounterComponent {
    case class State(count: Int, clock: ClockComponent.State)
    sealed trait Msg
    object Msg {
      case object Increase                      extends Msg
      case object Decrease                      extends Msg
      case object Reset                         extends Msg
      case class Random(l: Long)                extends Msg
      case class HttpResponse(res: Response)    extends Msg
      case class Clock(msg: ClockComponent.Msg) extends Msg
    }

    val init = Component.init(
      State(0, clock.init),
    )

    val update = Component.update[Msg, State] {
      case s -> Msg.Increase   => s.copy(count = s.count + 1)
      case s -> Msg.Decrease   => s.copy(count = s.count - 1)
      case s -> Msg.Reset      => s.copy(count = 0)
      case s -> Msg.Clock(msg) => s.copy(clock = clock.update(msg, s.clock))
    }

    val command = Component.command[Msg, State] {
      case _ -> Msg.Reset      =>
        Command.randomInt.map(Msg.Random(_)) *> Command.request("http://google.com").map(Msg.HttpResponse(_))
      case s -> Msg.Clock(msg) =>
        clock.command(msg, s.clock).map(Msg.Clock(_))
    }

    val cmd2Clock = Component.command[Msg, State] { case s -> Msg.Clock(msg) => clock.command(msg, s.clock).map(Msg.Clock(_)) }

    val counter = (CounterComponent.init &&& CounterComponent.update &&& CounterComponent.command)
      .install(clock)({ case s -> Msg.Clock(msg) => (s.clock, msg) })
  }

  object ClockComponent {
    case class State(time: Int, started: Boolean)

    sealed trait Msg
    object Msg {
      case object Tick  extends Msg
      case object Start extends Msg
      case object Stop  extends Msg
    }

    val init = Component.init(State(0, false))

    val update = Component.update[Msg, State] {
      case s -> Msg.Tick  => s.copy(time = s.time + 1)
      case s -> Msg.Start => s.copy(started = true)
      case s -> Msg.Stop  => s.copy(started = false)
    }

    val command = Component.command[Msg, State] {
      case _ -> Msg.Start             => Command.succeed(Msg.Tick)
      case State(_, true) -> Msg.Tick => Command.delay(1000).as(Msg.Tick)
    }

  }

  val clock = ClockComponent.init &&& ClockComponent.update &&& ClockComponent.command
}
