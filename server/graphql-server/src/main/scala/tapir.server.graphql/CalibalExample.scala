package tapir.server.graphql

import BaseEndpoints._
import caliban.schema.GenericSchema
import caliban.GraphQL._
import caliban.schema.Annotations.{GQLDeprecated, GQLDescription}
import caliban.schema.GenericSchema
import caliban.RootResolver
import zio.clock.Clock
import zio.console.Console
import zio.URIO

class CalibalExample extends App with GenericSchema[Console with Clock] {

  import EndpointToCaliban._

  implicit val countrySchema = gen[Country]
  implicit val authorSchema = gen[Author]
  implicit val genreSchema = gen[Genre]
  implicit val bookSchema = gen[Book]

  implicit val inputSchema = booksListing.toInputSchema()

  case class Queries(
      @GQLDescription("Return all characters from a given origin")
      listBooks: Limit => URIO[Console, Seq[Book]]
  )

  implicit val queriesSchema = gen[Queries]

  val lib = new Library

  val resolver = RootResolver(
    Queries(limit => URIO.apply(lib.getBooks(limit.getOrElse(1))))
  )
  val gql = graphQL[Any, Queries, Unit, Unit](resolver)

  println("----- Schema START -----")
  println(gql.render)
  println("----- Schema END -----")

}
