import $ivy.`dev.zio::zio:1.0.3`

import zio._
import zhttp.http._
import zhttp.http.HttpError.InternalServerError
import zhttp.service.Server
import zio.clock.Clock

import java.time.DateTimeException

object HelloWorld extends App {
  val app = Http.collectM[Request] {
    case Method.GET -> Root / "hello" =>
      HelloWorldService.helloWorld.fold({
        //error case(s) :
        case dateTimeError: DateTimeException =>
          Response.fromHttpError(InternalServerError("Internal error : " + dateTimeError.getMessage))
      },
        // success case :
        message => Response.text(message)
      )
  }

  override def run(args: List[String]) = {
    val serverIO = Server.start(8090, app).provideLayer(Clock.live) // <- bind clock implementation
    serverIO.exitCode
  }
}

object HelloWorldService {

  val clock = ZIO.access[Clock](_.get) // <- dependency injection : can use Clock.live, or a fake clock for tests

  def helloWorld: ZIO[Clock, DateTimeException, String] =
    for {
      c <- clock
      time <- c.localDateTime
    } yield s"Hello world, the current date is $time"
}