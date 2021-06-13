import $ivy.`dev.zio::zio:1.0.8`

import zio.{Promise, Runtime, UIO}

def program = for {
  p <- zio.Promise.make[Nothing, Double]
  _ <- p.completeWith(UIO(Math.random() * 1000))
  a <- p.await
  b <- p.await
} yield println(s"${a.toInt}, ${b.toInt}")

Runtime.default.unsafeRun(program)
