// scala 2.13.3
// ammonite 2.2.0

import $ivy.`dev.zio::zio:1.0.3`
import $ivy.`dev.zio::zio-streams:1.0.3`

import zio._
import zio.stream._
import zio.duration._

import scala.language.postfixOps

val fa = for {
  _ <- ZManaged.make(console.putStrLn("START"))(_ => console.putStrLn("END"))
} yield ZStream.repeat("???").schedule(Schedule.spaced(1 second)).take(10)

val program = for {
  f <- ZStream.unwrapManaged(fa).mapM(console.putStrLn(_)).runDrain.fork
  _ <- f.interrupt.delay(5 second)
} yield ()

println("Starting")
Runtime.default.unsafeRunAsync_(program.exitCode)
