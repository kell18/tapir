package tapir.server.graphql

import BaseEndpoints._
import sangria.schema.{fields, ListType, ObjectType, OutputType, Schema, ValidOutType, Value}
import scala.concurrent.{Await, Future}


import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

object SangriaExample extends App with SprayJsonSupport with DefaultJsonProtocol {
  import EndpointToSangria.RichSangriaEndpoint
  import sangria.macros.derive._

  implicit val countryType = deriveObjectType[Unit, Country](ObjectTypeDescription("The country"))
  implicit val authorType = deriveObjectType[Unit, Author](ObjectTypeDescription("The author"))
  implicit val genreType = deriveObjectType[Unit, Genre](ObjectTypeDescription("The genre"))
  implicit val bookType: ObjectType[Unit, Book] = deriveObjectType[Unit, Book](ObjectTypeDescription("The book"))
  implicit val booksType: ListType[Book] = ListType(bookType)

  // Concept of endpoints to ObjectTypes transformation - via the Sangria.Field ...
  // Sangria.Field is similar to the endpoint: input (args) + output (resolve method on context and that strange Val)
  val lsitBooks = booksListing.toSangriaField[Library, Any] { case (arg, ctx) =>
    Value[Library, Seq[Book]](ctx.ctx.getBooks(ctx.arg(arg)))
  }

  // ... ObjectType is top abstraction in sangria responsible for providing graph-unit, in this case it's a query unit;
  //     So that way we group endpoints around shared resources, for example, Library-related endpoints:
  val queryType = ObjectType("Query", fields[Library, Any](
    lsitBooks
    // TODO getBook, addBook, deleteBook
  ))

  val schema = Schema(queryType)

  println("----- Schema START -----")
  println(schema.renderPretty)
  println("----- Schema END -----")

  import sangria.macros._
  import sangria.execution._
  import sangria.marshalling.circe._
  import io.circe.Json
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val gqlQuery =
  graphql"""
    query MyBooks {
      BooksListing(limit: 2) {
        title
        year
        genre {
          name
        }
      }
    }
  """

  // TODO For now I don't understand what is root and how that Val type param of Sagria.Schema could be
  //      used (which is related to root somehow)
  val future: Future[Json] = Executor.execute(schema, gqlQuery, new Library, root = ())
  val result = Await.result(future, 10.seconds)

  println()
  println()
  println("----- Query results -----")
  println(result)

}