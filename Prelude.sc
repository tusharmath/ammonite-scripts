import $ivy.`dev.zio::zio:1.0.3`

def measure[A](name: String)(A: => A, n: Int = 1000) = {
  // warm up
  for (_ <- 0 to n) yield {
    A
  }

  // benchmark
  val durations = for (_ <- 0 to n) yield {
    val start = System.nanoTime
    A
    System.nanoTime - start
  }

  val ops = n.toDouble / durations.sum * 1000_000

  println(s"${name.padTo(15, ' ')}: ${Math.round(ops)} ops/sec")
}

val size = 1000
val l1 = List.from(0 to size)
val l2 = List.from(size * 1 + 1 to size * 2)
val l3 = List.from(size * 2 + 1 to size * 3)
val l4 = List.from(size * 3 + 1 to size * 4)
val l5 = List.from(size * 4 + 1 to size * 5)
val ll = List(l1, l2, l3, l4, l5)

def reverseFlatten[A](ll: List[List[A]]): List[A] = {
  var l = List.empty[A]
  var lli = ll
  while (!lli.isEmpty) {
    var li = lli.head
    while (!li.isEmpty) {
      l = li.head :: l
      li = li.tail
    }
    lli = lli.tail
  }
  l
}

// println((l1 ++ l2))
// println((l1 .map(_ => l2)).mkString("\n"))
// println(ll.flatMap(identity))
// println(l1 ++ l2 ++ l3 ++ l4 ++ l5)
// println(concat(ll))

measure("flatMap") { ll.flatMap(identity) }
measure("List.concat") { List.concat(l1, l2, l3, l4, l5) }
measure("reverseFlatten") { reverseFlatten(ll) }
measure("flatten") { ll.flatten }
measure("concat") { l1 ++ l2 ++ l3 ++ l4 ++ l5 }

// measure("++") { l1.flatMap(_ => _) }
// measure(":::") { l1 ::: l2 }
