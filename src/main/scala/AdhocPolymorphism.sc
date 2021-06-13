trait Animal
trait Dog extends Animal

def foo(a: Animal)

foo(dog) // sub-type


// parametric


def foo(list: List[Animal]) = ???

def map[A](list: List[A]) =  ???

// Adhoc

def map[F[_], A](fa: F[A], ab: A => B)(implicit impl: MappableOption[F]): F[B] = impl.performaMap(fa, ab)

sealed trait Expr[A] {
  def map[B](ab: Expr[A] => Expr[B])(implicit impl: MappableOption[A]) = impl.performaMap(self, ab)
}


sealed trait Mappable[F[_]] {
  def performMap[A, B](fa: F[A], ab: A => B)
}

object Mappable {
  implicit case object MappableOption extends Mappable[Option] {
    def performMap[A, B](fa: Option[A]) = ???
  }
  implicit case object MappableList extends Mappable[List[_]]
}

