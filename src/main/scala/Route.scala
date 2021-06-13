object Route {
  import scala.util.Try
  trait Named[A] {
    val name: String
  }

  val a: Int = 1
// object Named {
//   def apply[A](value: String) = new Named[A] {
//     val name: String = value
//   }

//   implicit val NameString: Named[String] = Named("str")
//   implicit val NameInt: Named[Int] = Named("int")
//   implicit val NameBoolean: Named[Boolean] = Named("bool")
//   implicit val NameDouble: Named[Double] = Named("dbl")
//   implicit val NameUnit: Named[Unit] = Named("unit")

//   implicit def Tuple2[A: Named, B: Named]: Named[(A, B)] = {
//     val a = implicitly[Named[A]]
//     val b = implicitly[Named[B]]

//     Named(s"${a.name}, ${b.name}")
//   }

//   implicit class NameSyntax[A](a: A) {
//     def name(implicit ev: Named[A]): String = ev.name
//   }
// }

// import Named._

// println("ABC".name)
// println(1.name)
// println((1, "A").name)
// println((1, ("A", (1, (1.0, (true, ()))))).name)

// trait DynDisp[E] {
//   type Out
//   def dispatch(name: String, msg: String): Option[Out]
// }

// trait Parser[A] {
//   def parse(s: String): Option[A]
// }

// type EOL = Unit

// implicit val EOLDynDisp: DynDisp[EOL] = new DynDisp[EOL] {
//   type Out = Nothing
//   override def dispatch(name: String, msg: String): Option[Nothing] = None
// }

// implicit val IntParser = new Parser[Int] {
//   def parse(s: String): Option[Int] = Try(s.toInt).toOption
// }

// implicit val StringParser = new Parser[String] {
//   def parse(s: String): Option[String] = Option(s)
// }
// implicit val BooleanParser = new Parser[Boolean] {
//   def parse(s: String): Option[Boolean] = Try(s.toBoolean).toOption
// }

// implicit def InductionDynDisp[A, B](implicit a: Named[A], p: Parser[B], b: DynDisp[B]): DynDisp[(A, B)] =
//   new DynDisp[(A, B)] {
//     type Out = Either[a.Out, B]

//     override def dispatch(name: String, msg: String): Option[Either[a.Out, A]] =
//       if (name == b.name)
//         p.parse(msg).map(Right(_))
//       else
//         b.dispatch(name, msg).map(Left(_))
//   }

// val a = implicitly[DynDisp[(Boolean, EOL)]]
// println(a.dispatch("bool", "true"))

}
