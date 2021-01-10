import $ivy.`dev.zio::zio:1.0.3`
import $ivy.`dev.zio::zio-streams:1.0.3`

type Method
type URL
type Path
type Endpoint = (Method, URL)
type Route = (Method, Path)

case class Http(run: Request => Response)

type Request
type Response
