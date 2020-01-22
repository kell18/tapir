package tapir.server.graphql

import Endpoints._
import sangria.marshalling.FromInput
import sangria.schema.{ListType, ObjectType, OutputType, ValidOutType, Value}

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

  /*val booksListingByGenre: Endpoint[BooksQuery, String, Vector[Book], Nothing] = baseEndpoint.get
    .in(("list" / path[String]("genre").map(Some(_))(_.get)).and(limitParameter).mapTo(BooksQuery))
    .out(jsonBody[Vector[Book]])*/
}

object BooksExample extends App {
  import EndpointToSangria.RichSangriaEndpoint
  import Endpoints._
  import sangria.macros.derive._
  import akka.http.scaladsl.server.Route

  implicit val countryType = deriveObjectType[Unit, Country](ObjectTypeDescription("The country"))
  implicit val authorType = deriveObjectType[Unit, Author](ObjectTypeDescription("The author"))
  implicit val genreType = deriveObjectType[Unit, Genre](ObjectTypeDescription("The genre"))
  implicit val bookType: ObjectType[Unit, Book] = deriveObjectType[Unit, Book](ObjectTypeDescription("The book"))
  implicit val booksType: ListType[Book] = ListType(bookType)

  // implicit val outType: OutputType[Vector[Book]] = ???
  implicit val validOut: ValidOutType[Int, Seq[Book]] = ???
  implicit val fromInput: FromInput[Int] = ???

  val books = booksListing.toSangriaField[Library](x => Value[Library, Seq[Book]](x.ctx.getAllBooks))(
    booksType,
    validOut,
    fromInput
  )
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
