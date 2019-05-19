package tapir.server.graphql

import akka.http.scaladsl.server.Route
import sangria.schema.{Action, Context}
import tapir.server.ServerEndpoint
import scala.concurrent.Future

object Resolvable {

  trait Resolvable[Val, Ctx] extends (Context[Ctx, Val] => Action[Ctx, _])

  type Name = String

  case class NamedResolvers[Ctx, Val](resolvers: Map[Name, Resolvable[Val, Ctx]])
}

// ..
// 1. Make dirty version
// 2. Submit PR and discuss: is it worth it?
case class EndpointToGraphQl(serverOpts: GraphQlServerOptions) {
  import tapir.server.graphql.Resolvable._

  private val log = getLogger

  def toObjectType[I, E, O, Ctx, Val](
      se: ServerEndpoint[I, E, O, AkkaStream, Future]
  )(resolvers: NamedResolvers[Ctx, Val]): Route  = {

  }
}
