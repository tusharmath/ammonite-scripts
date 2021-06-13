object PhantomVariance {

  type >:>[A, B] = B <:< A
  sealed abstract class MyModel[P, I, -T, +V]

  final case class Running[I, T, V](run: (I, T) => V) extends MyModel[String, I, T, V]
  final case class Inject[I](config: I)               extends MyModel[Int, I, Any, Nothing]

  sealed abstract class Gimme[+P]
  object Gimme {
    case object AStr  extends Gimme[String]
    case object AnInt extends Gimme[Int]
    def gimme[P](self: Gimme[P]): P =
      self match {
        case AStr  =>
          implicitly[P >:> String]
          // implicitly[String <:< P]
          "Sd"
        case AnInt => 0
      }
  }

  sealed abstract class Gotme[-P]

  object Gotme {
    final case object UnStr extends Gotme[String]
    final case object UnInt extends Gotme[Int]

    def execute[P](g: Gotme[P]): P => Unit = g match {
      case UnStr => (s: String) => ()
      case UnInt => (s: Int) => ()
    }
  }

  // def uncons[A](as: List[A]): Option[::[A]] = as match {
  //   case c @ (_ :: _) => Some(c)
  //   //                      ↑
  //   // [error] type mismatch;
  //   //  found   : ::[?A1] where type ?A1 <: A
  //   //            (this is a GADT skolem)
  //   //  required: ::[A]
  //   case _            => None
  // }

  sealed abstract class MyList[+A]
  final case class MyCons[A](head: A, tail: MyList[A]) extends MyList[A]
  final case class MyNil[A]()                          extends MyList[A]

  def drop1[A](as: MyList[A]): MyList[A] =
    as match {
      case MyNil()       => MyNil()
      case MyCons(_, tl) => tl
      // tl: MyList[L]  (L is a GADT skolem)
      // L <: A, therefore
      // MyList[L] <: MyList[A] by covariance
    }

}
