package tapir.server.graphql

import zio.DefaultRuntime
import zio.console._

object CalobanProblems extends App {
  import caliban.GraphQL.graphQL
  import caliban.RootResolver
  import caliban.schema._
  import caliban.GraphQL._

  import caliban.GraphQL._
  import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
  import caliban.schema.GenericSchema
  import org.http4s.implicits._


  // .. We need a way to describe parameters runtime. Or! - double-check the same but with generic types.
  case class Id(i: Int)
  case class Book(id: Int)
  case class Query(books: Id => Book)

  val gq = graphQL(RootResolver(Query(i => Book(i.i))))

  println(gq.render)

  println()
  println()
  println()

  val runtime = new DefaultRuntime {}
  val r = gq.execute(
    """
      |{
      |  books(i: 123) {
      |    id
      |  }
      |}
      |""".stripMargin).map(println)

  runtime.unsafeRun(r)
}
