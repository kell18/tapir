package tapir.server.graphql

import Endpoints._
import sangria.schema.{fields, ListType, ObjectType, OutputType, Schema, ValidOutType, Value}
import scala.concurrent.{Await, Future}

object SangriaExample

case class Country(name: String)
case class Author(name: String, country: Country)
case class Genre(name: String, description: String)
case class Book(title: String, genre: Genre, year: Int, author: Author)
case class BooksQuery(genre: Option[String], limit: Limit)

class BooksRepo {}

/**
  * Descriptions of endpoints used in the example.
  */
object Endpoints {
  import io.circe.generic.auto._
  import sttp.tapir._
  import sttp.tapir.json.circe._

  type Limit = Option[Int]
  type AuthToken = String

  // All endpoints report errors as strings, and have the common path prefix '/books'
  private val baseEndpoint = endpoint.errorOut(stringBody).in("books")

  // The path for this endpoint will be '/books/add', as we are using the base endpoint
  val addBook: Endpoint[(Book, AuthToken), String, Unit, Nothing] = baseEndpoint.post
    .in("add")
    .in(
      jsonBody[Book]
        .description("The book to add")
        .example(Book("Pride and Prejudice", Genre("Novel", ""), 1813, Author("Jane Austen", Country("United Kingdom"))))
    )
    .in(header[String]("X-Auth-Token").description("The token is 'secret'"))

  // Re-usable parameter description
  private val limitParameter = query[Int]("limit").description("Maximum number of books to retrieve")

  val booksListing: Endpoint[Int, String, Seq[Book], Nothing] = baseEndpoint.get
    .in("list" / "all")
    .in(limitParameter)
    .out(jsonBody[Seq[Book]])
    .info(EndpointInfo(Some("BooksListing"), None, None, Vector.empty))

  /*val booksListingByGenre: Endpoint[BooksQuery, String, Vector[Book], Nothing] = baseEndpoint.get
    .in(("list" / path[String]("genre").map(Some(_))(_.get)).and(limitParameter).mapTo(BooksQuery))
    .out(jsonBody[Vector[Book]])*/
}

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

object BooksExample extends App with SprayJsonSupport with DefaultJsonProtocol {
  import EndpointToSangria.RichSangriaEndpoint
  import Endpoints._
  import sangria.macros.derive._
  import akka.http.scaladsl.server.Route

  implicit val countryFmt = jsonFormat1(Country.apply)
  implicit val authorFmt = jsonFormat2(Author.apply)
  implicit val genreFmt = jsonFormat2(Genre.apply)
  implicit val bookFmt = jsonFormat4(Book.apply)

  implicit val countryType = deriveObjectType[Unit, Country](ObjectTypeDescription("The country"))
  implicit val authorType = deriveObjectType[Unit, Author](ObjectTypeDescription("The author"))
  implicit val genreType = deriveObjectType[Unit, Genre](ObjectTypeDescription("The genre"))
  implicit val bookType: ObjectType[Unit, Book] = deriveObjectType[Unit, Book](ObjectTypeDescription("The book"))
  implicit val booksType: ListType[Book] = ListType(bookType)

  // implicit val outType: OutputType[Vector[Book]] = ???
  // implicit val validOut: ValidOutType[Int, Seq[Book]] = ???
  // implicit val fromInput: FromInput[Int] = FromInput.defaultInput[Int]

  val books = booksListing.toSangriaField[Library, Any] { case (arg, ctx) =>
    Value[Library, Seq[Book]](ctx.ctx.getBooks(ctx.arg(arg)))
  }

  val query = ObjectType("Query", fields[Library, Any](books))

  val schema = Schema(query)

  println(schema.renderPretty)

  import sangria.macros._
  import sangria.execution._
  import sangria.marshalling.circe._
  import io.circe.Json
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val q =
  graphql"""
    query MyBooks {
      BooksListing(limit: 2) {
        title
        year
      }
    }
  """

  // .. on future - find out how to use Val
  val future: Future[Json] = Executor.execute(schema, q, new Library, root = ())
  val result = Await.result(future, 10.seconds)

  println("")
  println("-----------")
  println("")
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

  def getBooks(limit: Int) =
    getAllBooks.take(limit)

  /*def getBooks(query: BooksQuery): Seq[Book] = {
    val allBooks = Books.get()
    val limitedBooks = query.limit match {
      case None    => allBooks
      case Some(l) => allBooks.take(l)
    }
    val filteredBooks = query.genre match {
      case None    => limitedBooks
      case Some(g) => limitedBooks.filter(_.genre.name.equalsIgnoreCase(g))
    }
    filteredBooks
  }*/
}
