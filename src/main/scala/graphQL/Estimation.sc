// scala 2.13.3
// ammonite 2.2.0

import $ivy.`com.github.nscala-time::nscala-time:2.26.0`
import $ivy.`dev.zio::zio-nio-core:1.0.0-RC10`
import $ivy.`dev.zio::zio-nio:1.0.0-RC10`
import $ivy.`org.scalanlp::breeze:1.1`
import zio._
import zio.stream._
import zio.blocking.Blocking

import zio.nio.core._
import zio.nio.core.channels._
import zio.nio.core.file.Path
import zio.nio.file._

import breeze._
import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.time.ZonedDateTime
import java.time.OffsetDateTime
import java.time.LocalDateTime
import breeze.linalg._

val fmr = DateTimeFormatter.ISO_DATE_TIME

def extractData(path: Path) =
  for {
    lines <- Files.readAllLines(path)
    data = lines
      .map(_.split(",").toList)
      .slice(1, Int.MaxValue)
      .groupMapReduce({ case List(_, _, time, _) => time }) { i => List(i.last) }({ case (a, b) => a ++ b })
      .toList
      .map[(Double, Double, Double)]({
        case (time, rpm :: cpuA :: cpuB :: Nil) =>
          val rlStatus = if (LocalDateTime.parse(time, fmr).getHour > 14) 1 else 0
          (rlStatus.toDouble, rpm.toDouble, Math.max(cpuA.toDouble, cpuB.toDouble))

        case (time, rpm :: cpu :: Nil) =>
          val rlStatus = if (LocalDateTime.parse(time, fmr).getHour > 14) 1 else 0
          (rlStatus.toDouble, rpm.toDouble, cpu.toDouble)
      })
  } yield data.slice(1, Int.MaxValue)

def regress(observations: DenseMatrix[Double], outputs: DenseVector[Double]): DenseVector[Double] = {
  val cov    = DenseMatrix.zeros[Double](observations.cols, observations.cols) + (observations.t * observations)
  val scaled = DenseVector.zeros[Double](observations.cols) + (observations.t * outputs)
  cov \ scaled
}

def predictCpu(rpm: Double)(list: List[(Double, Double, Double)]) = {
  val observations = DenseMatrix(list.map({ case (rl, rpm, cpu) => (rl, rpm) }): _*)
  val outputs      = DenseVector(list.map({ case (rl, rpm, cpu) => cpu }).toArray)
  val scale        = regress(observations, outputs)
  val cpu          = DenseMatrix((1d, rpm)) * scale
  cpu.toArray.head
}

def program = for {
  d1 <- extractData(Path("graphQL/extract-2021-04-14T12_19_50.385Z-RPM.csv")).map(_.map({ case (rl, rpm, cpu) => (rl, rpm / 3640, cpu) }))
  d2 <- extractData(Path("graphQL/extract-2021-04-14T12_27_00.990Z-RPM.csv")).map(_.map({ case (rl, rpm, cpu) => (rl, rpm / 2810, cpu) }))
  d3 <- extractData(Path("graphQL/extract-2021-04-14T12_33_14.462Z-RPM.csv")).map(_.map({ case (rl, rpm, cpu) => (rl, rpm / 2230, cpu) }))
  d           = d1 ++ d2 ++ d3
  expectedRPM = 33_000
  cpu0        = predictCpu(expectedRPM)(d1).formatted("%.2f")
  cpu1        = predictCpu(expectedRPM)(d2).formatted("%.2f")
  cpu2        = predictCpu(expectedRPM)(d3).formatted("%.2f")
  cpu         = predictCpu(expectedRPM)(d).formatted("%.2f")
  _ <- console.putStrLn(s"${cpu0}% + ${cpu1}% + ${cpu2}% = ${cpu}%")
} yield ()

stats.regression.lasso

Runtime.default.unsafeRunAsync_(program)
