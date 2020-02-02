package tapir.server.graphql

import Endpoints._
import sangria.schema.{fields, ListType, ObjectType, OutputType, Schema, ValidOutType, Value}
import scala.concurrent.{Await, Future}

case class Country(name: String)
case class Author(name: String, country: Country)
case class Genre(name: String, description: String)
case class Book(title: String, genre: Genre, year: Int, author: Author)
case class BooksQuery(genre: Option[String], limit: Limit)

object Endpoints {
  import io.circe.generic.auto._
  import sttp.tapir._
  import sttp.tapir.json.circe._

  type Limit = Option[Int]

  // All endpoints report errors as strings, and have the common path prefix '/books'
  private val baseEndpoint = endpoint.errorOut(stringBody).in("books")

  // Re-usable parameter description
  private val limitParameter = query[Int]("limit").description("Maximum number of books to retrieve")

  val booksListing: Endpoint[Int, String, Seq[Book], Nothing] = baseEndpoint.get
    .in("list" / "all")
    .in(limitParameter)
    .out(jsonBody[Seq[Book]])
    .info(EndpointInfo(Some("BooksListing"), None, None, Vector.empty, deprecated = false))
}

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

object SangriaExample extends App with SprayJsonSupport with DefaultJsonProtocol {
  import EndpointToSangria.RichSangriaEndpoint
  import Endpoints._
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

class Library {
  import java.util.concurrent.atomic.AtomicReference

  val Books = new AtomicReference(
    Seq(
      Book(
        "The Sorrows of Young Werther",
        Genre("Novel", "Novel is genre"),
        1774,
        Author("Johann Wolfgang von Goethe", Country("Germany"))
      ),
      Book("Iliad", Genre("Poetry", ""), -8000, Author("Homer", Country("Greece"))),
      Book("Nad Niemnem", Genre("Novel", ""), 1888, Author("Eliza Orzeszkowa", Country("Poland"))),
      Book("The Colour of Magic", Genre("Fantasy", ""), 1983, Author("Terry Pratchett", Country("United Kingdom"))),
      Book("The Art of Computer Programming", Genre("Non-fiction", ""), 1968, Author("Donald Knuth", Country("USA"))),
      Book("Pharaoh", Genre("Novel", ""), 1897, Author("Boleslaw Prus", Country("Poland")))
    )
  )

  def getAllBooks: Seq[Book] = Books.get()

  def getBooks(limit: Int) = getAllBooks.take(limit)

}
