// scala 2.13.3
// ammonite 2.2.0

type Dom
type Response

sealed trait Component[A, S] { self =>
  def &&&[A1](other: Component[A1, S])(implicit ev: A1 <:< A): Component[A1, S] =
    Component.Combine(self.asInstanceOf[Component[A1, S]], other)
    
  def init: S                                                                   = ???
  def update(a: A, s: S): S                                                     = ???
  def command(a: A, s: S): Command[A]                                           = ???
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
}

sealed trait Command[+A] { self =>
  def ::[A1 >: A](other: Command[A1]): Command[A1] = Command.Combine(self, other)
  def flatMap[B](f: A => Command[B]): Command[B]   = Command.FlatMap(self, f)
  def map[B](ab: A => B): Command[B]               = self.flatMap(a => Command.succeed(ab(a)))
  def dispatchWith[B](ab: A => B): Command[B]      = self.flatMap(a => Command.msg(ab(a)))
  def dispatch[B](b: B): Command[B]                = self.flatMap(_ => Command.msg(b))
}

object Command extends TimerCommands with RandomCommands with HttpCommands with ComponentCommands {
  case object Empty                                           extends Command[Nothing]
  case class Succeed[A](a: A)                                 extends Command[A]
  case class Combine[A](l: Command[A], r: Command[A])         extends Command[A]
  case class FlatMap[A, B](c: Command[A], f: A => Command[B]) extends Command[B]

  def empty: Command[Nothing]      = Command.Empty
  def succeed[A](a: A): Command[A] = Command.Succeed(a)
}

trait ComponentCommands {
  case class Message[A](msg: A) extends Command[A]
  def msg[A](m: A): Command[A] = Message(m)
}

trait TimerCommands {
  case class Delay[A](duration: Long) extends Command[Unit]
  def delay(duration: Long): Command[Unit] = Delay(duration)
}

trait RandomCommands {
  case object GenerateRandom extends Command[Long]
  def randomLong: Command[Long] = GenerateRandom
  def randomInt: Command[Int]   = randomLong.map(_.toInt)
  def randomChar: Command[Char] = randomLong.map(i => (i.toInt % 26).toChar)
}

trait HttpCommands {
  case class Request[A](url: String, f: Response => A) extends Command[A]
  def request[A](url: String, f: Response => A): Command[A] = Request(url, f)
}

// Example Usage

object Example {

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

    val init = Component.init(State(0, clock.init))

    val update = Component.update[Msg, State] {
      case s -> Msg.Increase   => s.copy(count = s.count + 1)
      case s -> Msg.Decrease   => s.copy(count = s.count - 1)
      case s -> Msg.Reset      => s.copy(count = 0)
      case s -> Msg.Clock(msg) => s.copy(clock = clock.update(msg, s.clock))
    }

    val command = Component.command[Msg, State] {
      case _ -> Msg.Reset      => Command.randomInt.dispatchWith(Msg.Random(_)) :: Command.request("http://google.com", Msg.HttpResponse(_))
      case s -> Msg.Clock(msg) => clock.command(msg, s.clock).dispatchWith(Msg.Clock(_))
    }

  }
  val counter = CounterComponent.init &&& CounterComponent.update &&& CounterComponent.command

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

    val command = Component.command[Msg, State]({ case State(_, true) -> Msg.Tick =>
      Command.delay(1000).dispatch(Msg.Tick)
    })

  }

  val clock = ClockComponent.init &&& ClockComponent.update &&& ClockComponent.command
}
