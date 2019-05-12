package tapir.example

import java.util.Properties
import com.typesafe.scalalogging.StrictLogging
import sangria.schema.{Action, Context}
import tapir.example.Endpoints.Limit
import tapir.internal.server.{DecodeInputs, DecodeInputsContext, DecodeInputsResult}
import tapir.{Endpoint, EndpointIO}
import tapir.{Schema => TSchema}
import tapir.EndpointIO.Info
import tapir.Schema.{SObject, SString}
import scala.concurrent.Future
import scala.reflect.internal.annotations

case class Country(name: String)
case class Author(name: String, country: Country)
case class Genre(name: String, description: String)
case class Book(title: String, genre: Genre, year: Int, author: Author)
case class BooksQuery(genre: Option[String], limit: Limit)


object GraphQLResource {
  import sangria.schema.ObjectType
  import sangria.schema.Schema
  import sangria.schema.fields
  import sangria.schema.Field
  import sangria.schema.OutputType
  import sangria.schema.{StringType, IntType, ObjectType}



  // @annotations.tailrecursive
  def makeFields(fieldsSchema: TSchema): OutputType[_] = fieldsSchema match {
    case s@SString => Field(s.show, StringType)
    case SObject(objInfo, fields, required) => {
      ObjectType(
        objInfo.fullName,
        fields.map { case (fName, fSchema) => Field(fName, makeFields(fSchema)) }
      )
    }
  }

  // @annotations.tailrecursive
  def makeMutation(fieldsSchema: TSchema): OutputType[_] = fieldsSchema match {
    case s@SString =>
    case SObject(objInfo, fields, required) =>
  }

  def makeObjectType[T, Ctx, Val](tSchema: TSchema, info: Info[T]): ObjectType[Ctx, Val] = {
    ObjectType(
      name,
      info.description,
      makeFields(tSchema)
    )
  }

  case class InvalidEndpoint(reason: String) extends Exception(reason)

  case class ResolverNotFound(name: String) extends Exception(s"Cannot find GraphQL resolver $name.")

  // .. general architectural idea (apply this after it'll be possible to generate GQL bellow):
  // def resolvers: Map[Name, Resolvable[Val, Ctx]] = ???
  //
  // ep.interpret { inputs =>
  //   // do whatever you want with inputs
  //   Utils.ToGraphQL(inputs)(resolvers)
  // }
  // OR ep.toGraphQL(resolvers)


  trait Resolvable[Val, Ctx] extends (Context[Ctx, Val] => Action[Ctx, _])

  type Name = String

  case class NamedResolvers[Ctx, Val](resolvers: Map[Name, Resolvable[Val, Ctx]])


  class ToGQL[I, E, O, S[_]](ep: Endpoint[I, E, O, S]) {
    def apply(logic: I => Future[Either[E, O]]): ObjectType[DecodeInputsContext, I] = {

      def decodeBody[Ctx, Val](resolvers: Map[Name, Resolvable[Val, Ctx]]): ObjectType[DecodeInputsContext, DecodeInputsResult] = {
        result match {
          case values: DecodeInputsResult.Values =>
            values.bodyInput match {
              case Some(bodyInput@EndpointIO.Body(codec, info)) => {
                val name = ep.info.name.getOrElse(throw InvalidEndpoint("Endpoint name is required to generate GraphQL"))

                val in: ObjectType[Ctx, I] = ???

                // .. find a name for the all fields
                codec.meta.schema match {
                  case s@SString => Field(s.show, StringType)
                  case SObject(objInfo, fields, required) => {
                    ObjectType(
                      objInfo.fullName,
                      fields.map { case (fName, fSchema) =>
                        // .. double check
                        val r = resolvers.getOrElse(fName, throw ResolverNotFound(fName))
                        Field(fName, makeFields(fSchema), resolve = r)
                      }
                    )
                  }
                }

                val out = ObjectType(name, ep.info.description, fields[Ctx, O](

                ))

              }
            }
          case failure: DecodeInputsResult.Failure =>
        }

        ???
      }

      val context: DecodeInputsContext = ??? // .. Own wrapper?
      val decodedInput: DecodeInputsResult = DecodeInputs(ep.input, context)

      decodeBody(decodedInput) // .. set types right or at least cast type to I
      ???
    }
  }


  val name = "gql res"
  val descr = "blah blah blah"

  type F1type = String
  val f1name = ""
  val f1type: OutputType[String] = "f1TapirParams" match {
    case _ => StringType
  }

  val o = ObjectType(name, descr,
    fields[Unit, F1type](
      Field(f1name, f1type, resolve = _.value)
    )
  )
}

/**
  * Descriptions of endpoints used in the example.
  */
object Endpoints {
  import io.circe.generic.auto._
  import tapir._
  import tapir.json.circe._

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
  private val limitParameter = query[Option[Int]]("limit").description("Maximum number of books to retrieve")

  val booksListing: Endpoint[Limit, String, Vector[Book], Nothing] = baseEndpoint.get
    .in("list" / "all")
    .in(limitParameter)
    .out(jsonBody[Vector[Book]])

  val a = ("list" / path[String]("genre").map(Some(_))(_.get)).and(limitParameter).mapTo(BooksQuery)
  val booksListingByGenre: Endpoint[BooksQuery, String, Vector[Book], Nothing] = baseEndpoint.get
    .in(a)
    .out(jsonBody[Vector[Book]])
}

object BooksExample extends App with StrictLogging {

  import Endpoints._
  import akka.http.scaladsl.server.Route

  def openapiYamlDocumentation: String = {
    import tapir.docs.openapi._
    import tapir.openapi.circe.yaml._

    // interpreting the endpoint description to generate yaml openapi documentation
    val docs = List(addBook, booksListing, booksListingByGenre).toOpenAPI("The Tapir Library", "1.0")
    docs.toYaml
  }

  def booksRoutes: Route = {
    import akka.http.scaladsl.server.Directives._
    import tapir.server.akkahttp._

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future

    def bookAddLogic(book: Book, token: AuthToken): Future[Either[String, Unit]] = Future {
      if (token != "secret") {
        logger.warn(s"Tried to access with token: $token")
        Left("Unauthorized access!!!11")
      } else {
        logger.info(s"Adding book $book")
        Library.Books.getAndUpdate(books => books :+ book)
        Right(())
      }
    }

    def bookListingLogic(limit: Limit): Future[Either[String, Vector[Book]]] = Future {
      Right[String, Vector[Book]](Library.getBooks(BooksQuery(None, limit)))
    }

    def bookListingByGenreLogic(query: BooksQuery): Future[Either[String, Vector[Book]]] = Future {
      Right[String, Vector[Book]](Library.getBooks(query))
    }

    // interpreting the endpoint description and converting it to an akka-http route, providing the logic which
    // should be run when the endpoint is invoked.
    addBook.toRoute((bookAddLogic _).tupled) ~
      booksListing.toRoute(bookListingLogic) ~
      booksListingByGenre.toRoute(bookListingByGenreLogic)
  }

  def startServer(route: Route, yaml: String): Unit = {
    import akka.actor.ActorSystem
    import akka.http.scaladsl.Http
    import akka.http.scaladsl.server.Directives._
    import akka.stream.ActorMaterializer

    import scala.concurrent.Await
    import scala.concurrent.duration._
    val routes = route ~ new SwaggerUI(yaml).routes
    implicit val actorSystem: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    Await.result(Http().bindAndHandle(routes, "localhost", 8080), 1.minute)

    logger.info("Server started")
  }

  def makeClientRequest(): Unit = {

    import com.softwaremill.sttp._
    import tapir.client.sttp._

    implicit val backend: SttpBackend[Id, Nothing] = HttpURLConnectionBackend()

    val booksListingRequest: Request[Either[String, Vector[Book]], Nothing] = booksListing
      .toSttpRequest(uri"http://localhost:8080")
      .apply(Option(3))

    val result: Either[String, Vector[Book]] = booksListingRequest.send().unsafeBody

    logger.info("Result of listing request with limit 3: " + result)
  }

  logger.info("Welcome to the Tapir Library example!")

  logger.info("Starting the server ...")
  startServer(booksRoutes, openapiYamlDocumentation)

  logger.info("Making a request to the listing endpoint ...")
  makeClientRequest()

  logger.info("Try out the API by opening the Swagger UI: http://localhost:8080/swagger")
}

/**
  * Defines akka-http routes which serve the swagger ui (read from the webjar dependency) and the given yaml with
  * description of the book endpoints.
  */
class SwaggerUI(yml: String) {
  import akka.http.scaladsl.model.StatusCodes
  import akka.http.scaladsl.server.Directives._
  import akka.http.scaladsl.server.Route

  val SwaggerYml = "swagger.yml"

  private val redirectToIndex: Route =
    redirect(s"/swagger/index.html?url=/swagger/$SwaggerYml", StatusCodes.PermanentRedirect) //

  private val swaggerVersion = {
    val p = new Properties()
    p.load(getClass.getResourceAsStream("/META-INF/maven/org.webjars/swagger-ui/pom.properties"))
    p.getProperty("version")
  }

  val routes: Route =
    path("swagger") {
      redirectToIndex
    } ~
      pathPrefix("swagger") {
        path("") { // this is for trailing slash
          redirectToIndex
        } ~
          path(SwaggerYml) {
            complete(yml)
          } ~
          getFromResourceDirectory(s"META-INF/resources/webjars/swagger-ui/$swaggerVersion/")
      }
}

object Library {
  import java.util.concurrent.atomic.AtomicReference

  val Books = new AtomicReference(
    Vector(
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

  def getBooks(query: BooksQuery): Vector[Book] = {
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
  }
}
