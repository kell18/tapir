package tapir.server.graphql

object BaseEndpoints {

  case class Country(name: String)
  case class Author(name: String, country: Country)
  case class Genre(name: String, description: String)
  case class Book(title: String, genre: Genre, year: Int, author: Author)

  case class BooksQuery(genre: Option[String], limit: Limit)

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

}
