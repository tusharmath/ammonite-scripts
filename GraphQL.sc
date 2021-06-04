import ammonite.$file.GraphQL.Node.Empty
import ammonite.$file.GraphQL.Node.Entity
import ammonite.$file.GraphQL.Node.Property
import ammonite.$file.GraphQL.Node.Combine
import ammonite.$file.GraphQL.Kind.Literal
import ammonite.$file.GraphQL.Kind.Child
sealed trait Node { self =>
  def ++(other: Node): Node = Node.Combine(self, other)
  def entityName: String    =
    self match {
      case Entity(name, _) => name
      case _               => ""
    }

  def graphQLSchema: String =
    self match {
      case Empty                  => ""
      case Entity(name, children) =>
        s"""
        type ${name} {
          ${children.graphQLSchema}
        }
        """.stripMargin
      case Property(name, kind)   =>
        s"${name}: ${kind.asString}"
      case Combine(a, b)          =>
        s"""
          ${a.graphQLSchema}
          ${b.graphQLSchema}
        """
    }
}

object Node {
  case object Empty                               extends Node
  case class Entity(name: String, children: Node) extends Node
  case class Property(name: String, kind: Kind)   extends Node
  case class Combine(a: Node, b: Node)            extends Node

  def entity(name: String)(ch: Node): Node  = Entity(name, ch)
  def field(name: String, kind: Kind): Node = Property(name, kind)
  def empty: Node                           = Empty
}

sealed trait Kind { self =>
  def asString: String =
    self match {
      case Kind.Numeric => "Int"
      case Literal      => "String"
      case Child(n)     => n.entityName
    }
}
object Kind       {
  case object Numeric       extends Kind
  case object Literal       extends Kind
  case class Child(n: Node) extends Kind
}

import Node._

val address = entity("Address") {
  field("street", Kind.Literal) ++ field("pin", Kind.Numeric) ++ field("house", Kind.Literal)
}

val user = entity("User") {
  field("name", Kind.Literal) ++ field("id", Kind.Numeric) ++ field("address", Kind.Child(address))
}

println(user.graphQLSchema)

// case class Address(street: String, pin: Int, house: String)
// case class User(id: Int, name: String, address: Address)
// case class Root(user: User)
