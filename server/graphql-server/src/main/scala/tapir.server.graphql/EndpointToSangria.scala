package tapir.server.graphql

import sangria.ast
import sangria.marshalling.MarshallerCapability
import sangria.schema.{Schema => SangriaSchema, _}
import sangria.validation.{BigIntCoercionViolation, IntCoercionViolation}
import sttp.tapir.{Codec, CodecForMany, CodecFormat, CodecMeta, Endpoint, EndpointInfo, EndpointInput, Schema}
import sttp.tapir.SchemaType.{SInteger, SObjectInfo, SProduct, SString}
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

      // .. bigger than schemaToSInputType - because not every ep input contains Schema
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

    def schemaToSInputType[T: ClassTag](schema: Schema[T]): InputType[T] = {
      val tName = implicitly[ClassTag[T]].getClass.getSimpleName
      (schema match {
        case Schema(SString, _, description, _) =>
          ScalarType[String](
            tName,
            description,
            coerceUserInput = { any =>
              Right(any.toString)
            },
            coerceInput = { v =>
              Right(v.renderPretty)
            },
            // .. make sure it's correct
            coerceOutput = { case (t, _) => t.toString }
          ).asInstanceOf[ScalarType[T]]
        case Schema(SInteger, _, description, _) =>
          ScalarType[Int](
            tName,
            description,
            coerceUserInput = {
              case i: Int ⇒ Right(i)
              case i: Long if i.isValidInt ⇒ Right(i.toInt)
              case i: BigInt if !i.isValidInt ⇒ Left(BigIntCoercionViolation)
              case i: BigInt ⇒ Right(i.intValue)
              case d: Double if d.isValidInt ⇒ Right(d.intValue)
              case d: BigDecimal if d.isValidInt ⇒ Right(d.intValue)
              case _ ⇒ Left(IntCoercionViolation)
            },
            coerceInput = {
              case ast.IntValue(i, _, _) ⇒ Right(i)
              case ast.BigIntValue(i, _, _) if !i.isValidInt ⇒ Left(BigIntCoercionViolation)
              case ast.BigIntValue(i, _, _) ⇒ Right(i.intValue)
              case _ ⇒ Left(IntCoercionViolation)
            },
            coerceOutput = valueOutput
          )
        case Schema(SProduct(SObjectInfo(fullName, tParamsNames), fieldsS), isOptional, description, format) =>
          val fieldsInpTypes = fieldsS.map { case (name, fSchema) => name -> schemaToSInputType(fSchema) }
           // TODO provide at least descriptions and maybe defaultValues
          val inputFields = fieldsInpTypes.map { case (name, iType) => InputField(name, iType, "") }.toList
          InputObjectType(
            tName,
            description,
            () => inputFields,
            Vector.empty,
            Vector.empty
          )
        // ..
        case unknown => sys.error(s"$unknown input schema match is not implemented")
      }).asInstanceOf[ScalarType[T]]
    }

    // def schemaToSInputField(schema: Schema[_]):

    def valueOutput[T](value: T, capabilities: Set[MarshallerCapability]): T = value

    // .. In case we need obj type for each arg
    // def inputToArgsAndTypes(input: EndpointInput[I]): List[(ObjectType[Ctx, Val], Argument[_])] = ???
    /*match {
        case EndpointInput.Query(name, codec, info) =>
          val sType = codecToSType(codec)
          ???
      }*/
  }
}
