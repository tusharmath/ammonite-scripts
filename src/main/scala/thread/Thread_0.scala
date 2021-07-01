package thread

object Thread_0 extends App {
  var count = 0
  def updateCount = {
    var i = 100
    while (i > 0) {
      count = count + 1
      i = i - 1
    }
  }
  val th1   = new Thread(() => updateCount)
  val th2   = new Thread(() => updateCount)

  th1.run()
  th2.run()

  th1.join()
  th2.join()
  println(s"$count")
}
