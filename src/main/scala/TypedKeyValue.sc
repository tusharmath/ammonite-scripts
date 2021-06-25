import TypedKeyValue.IsKeyValuePair
import java.time.LocalDateTime
object TypedKeyValue {
  // trait IsKey[A] {
  //   type Value
  // }

  trait IsKeyValuePair[A, B]

  type ~>[A, B] = IsKeyValuePair[A, B]

  object IsKeyValuePair:
    def apply[A, B]: IsKeyValuePair[A, B] = new IsKeyValuePair[A, B] {}
    
  def derive[A, B] = IsKeyValuePair[A, B]

  

  sealed trait TMap:
    def put[A, B](key: A, value: B)(using ev: IsKeyValuePair[A, B]): TMap
    def get[A, B](key: A)(using ev: IsKeyValuePair[A, B]): Option[B]

  object TMap: 
    def empty: TMap = ???


  val map = TMap.empty

  
  
  given (String ~> Int) = derive
  given (Boolean ~> Int) = derive
  given (LocalDateTime ~> Int) = derive
  
  map.put("String", 1).put(true, 1)

  type FirstElement[T] =  T match
    case List[a]   => a
    case Option[a] => a
    case _         => Nothing
  
  def head[A](fa: A): FirstElement[A] = ???

  val a = head(Option(1))
}