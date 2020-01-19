package tapir.server.graphql

import sangria.schema.{Schema => SangriaSchema, _}
import sttp.tapir.{Codec, CodecForMany, CodecFormat, CodecMeta, Endpoint, EndpointInfo, EndpointInput, Schema}
import sttp.tapir.SchemaType.SString
import scala.reflect.ClassTag

object EndpointToSangria {
  type Resolver[Ctx, Val] = Context[Ctx, Val] ⇒ Action[Ctx, _]

  implicit class RichSangriaEndpoint[I, O, E](endpoint: Endpoint[I, O, E, _]) {
    /*def toSangriaQuery[Ctx](resolver: Resolver[Ctx, Any])(name: String)(
      implicit outType: ObjectType[Ctx, O]
    ): ObjectType[Ctx, _] = {
      val queryField = inputsToQueryField(name)(endpoint, resolver)
      ObjectType(name, fields[Ctx, Any](queryField))
    }*/

    // Query.Field is something like one endpoint - args + resolve
    def toSangriaField[Ctx, Val, Res](name: String)(ep: Endpoint[I, O, E, _], resolve: Context[Ctx, Any] => Action[Ctx, Res])(
        implicit outType: OutputType[O],
        ev: ValidOutType[Res, O]
    ): Field[Ctx, Any] = {
      def inputToArgs(input: EndpointInput[I]): List[Argument[_]] = {
        input match {
          case EndpointInput.Query(qName, qCodec, qInfo) =>
        }
        ???
      }

      val EndpointInfo(fName, summary, description, tags) = ep.info
      val args = inputToArgs(ep.input)
      Field(name, outType, description, args, resolve)
    }

    def argTypeFromCodec[T: ClassTag, CF <: CodecFormat, R](meta: CodecMeta[T, CF, R]): InputType[T] = {
      val tName = implicitly[ClassTag[T]].getClass.getSimpleName
      meta.schema match {
        case Schema(SString, _, description, _) =>
          ScalarType(tName, description, coerceUserInput = { any =>
            Right(any.toString)
          }, coerceOutput = { case (t, _) => t.toString }, coerceInput = { v =>
            Right(v.renderPretty)
          })
        // ..
      }
    }

    // .. In case we need obj type for each arg
    // def inputToArgsAndTypes(input: EndpointInput[I]): List[(ObjectType[Ctx, Val], Argument[_])] = ???
    /*match {
        case EndpointInput.Query(name, codec, info) =>
          val sType = codecToSType(codec)
          ???
      }*/
  }
}
