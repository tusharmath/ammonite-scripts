val sales = 0 to 10

val total = sales.sum
println(total)

val name = Thread.currentThread.getName
val id   = Thread.currentThread.getId()
println(id)

val thread = new Thread(
  () => {
    println(Thread.currentThread.getName)
  },
  "Calculation Thread",
)

thread.start
try {
  thread.join
} catch {
  case e => println(e)
}

println("Hello World!")
