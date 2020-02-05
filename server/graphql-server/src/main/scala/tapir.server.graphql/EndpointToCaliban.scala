package tapir.server.graphql

import akka.http.scaladsl.server.Route
import caliban.{CalibanError, GraphQL}
import caliban.introspection.adt.__Field
import caliban.schema.GenericSchema
import sangria.schema.{Action, Context}
import sttp.tapir.{endpoint, jsonBody, query, stringBody, Endpoint, EndpointInput, Schema => TSchema}
import sttp.tapir.SchemaType._
import zio.URIO
import scala.concurrent.Future
import scala.reflect.ClassTag

object Resolvable {

  trait Resolvable[Val, Ctx] extends (Context[Ctx, Val] => Action[Ctx, _])

  type Name = String

  case class NamedResolvers[Ctx, Val](resolvers: Map[Name, Resolvable[Val, Ctx]])
}


object EndpointToCaliban extends App with GenericSchema[Any] {
  import caliban.GraphQL.graphQL
  import caliban.RootResolver
  import caliban.schema._
  import caliban.schema.Types._
  import caliban.GraphQL._

  import caliban.GraphQL._
  import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
  import caliban.schema.GenericSchema
  import org.http4s.implicits._

  import io.circe.generic.auto._
  // import sttp.tapir._
  import sttp.tapir.json.circe._

  implicit class EndpointToCalibanSchema[I, E, O](endp: Endpoint[I, E, O, _]) extends GenericSchema[Any] {
    def toInputSchema(): Schema[Any, I] = {
      tInputToSchema(endp.input) match {
        case singleArg :: Nil => singleArg.asInstanceOf[Schema[Any, I]]
        case notSingleArg     => sys.error(s"This implementation supports exactly 1 arg, got: $notSingleArg")
      }
    }

    def tInputToSchema[I1](endpointInput: EndpointInput[I1]): List[Schema[Any, _]] = endp.input match {
        case q @ EndpointInput.Query(name, codec, info) => tScemaToCScema(name, codec.meta.schema) :: Nil
        case q @ EndpointInput.Multiple(inputs) =>
          inputs.flatMap(x => tInputToSchema(x.asInstanceOf[EndpointInput[_]])).toList
        case _ => ???
      }

    def tScemaToCScema(argName: String, tSchema: TSchema[_]): Schema[Any, _] = tSchema match {
        // TODO Use description and mb others
      case TSchema(SString, isOpt, description, _, _) => stringSchema.setIsOptional(isOpt)
      case TSchema(SInteger, isOpt, description, _, _) => intSchema.setIsOptional(isOpt)
      case TSchema(SProduct(info, fields), isOpt, description, _, _) =>
        val fieldsS: List[(__Field, _ => Step[Any])] = ???
        objectSchema(argName, None, fieldsS)
      case _ => ???
    }

    // def
  }

  implicit class CSchemaOps[R, T](schema: Schema[R, T]) extends GenericSchema[R] {
    def setIsOptional(b: Boolean): Schema[R, _] =
      if (b) optionSchema(schema) else schema


  }


  /*case class CharactersArgs(origin: Option[String])
  case class CharacterArgs(name: String)

  // schema
  case class CharacterName(name: String)
  case class Queries(
                      characters: CharactersArgs => List[Character],
                      character: CharacterArgs => Option[Character]
                    )
  // resolver
  val queries = Queries(_ => List.empty, a => Option(Character(a.name)))

  val interpreter: GraphQL[Any, Queries, Unit, Unit, CalibanError] = graphQL(RootResolver(queries))

  case class Query2[I1, I2, O1, O2](logic1: I1 => O1, logic2: I2 => O2)

  // implicit def qs[I1, O1](implicit qSchemaI1: Schema[Any, I1], qSchemaO1: Schema[Any, O1]) = gen[Query[I1, O1]]

  case class Query[T, O](logic1: T => O)

  val q1 = Query[CharacterArgs, Character](_ => Character("123"))

  // .. this is the way - move it to RichENdpoint and wrap up some basic draft PR! :)
  def graphQlFor[T, O](qq: Query[T, O])(implicit schema: Schema[Any, T => O]) = graphQL(RootResolver(qq))

  val i1 = graphQlFor(q1)

  // println(i1.render)

  case class Country(name: String)
  case class Author(name: String, country: Country)

  implicit class RichAkkaHttpEndpoint1[I1, E, O1, S](e: Endpoint[I1, E, O1, S])(
    implicit qSchema: Schema[Any, Query[I1, O1]]
  ) {

    def toRoute(logic1: I1 => O1)/*(
      implicit serverOptions: GraphQlServerOptions
    )*/: GraphQL[Any, Query[I1, O1], Unit, Unit, CalibanError] = {
      val r = RootResolver(Query(logic1))
      graphQL(r)
    }
  }

  val limitParameter = query[Option[Int]]("limit").description("Maximum number of books to retrieve")

  val ep = endpoint
    .errorOut(stringBody)
    .in("authors")
    .in(limitParameter)
    .out(jsonBody[List[Author]])

  val gq = ep.toRoute {
    case Some(limit) => Author("Michail Bulgakov", Country("USSR")) :: Nil
    case None => Author("Conan Doyle", Country("UK")) :: Nil
  }

  println(ep.show)
  println()
  println()

  println(gq.render)

  println()
  println()


  case class Limit(l: Int)
  case class ManQuery(authors: Limit => List[Author])
  val manGq = graphQL(RootResolver(ManQuery(x => Author("Michail Bulgakov", Country(x.toString)) :: Nil)))

  println(manGq.render)

  // Open questions:
  // 1. multi-arg endpoints
*/
}