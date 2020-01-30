package tapir.server.graphql

import sangria.ast
import sangria.marshalling.{FromInput, MarshallerCapability}
import sangria.schema.{Schema => SangriaSchema, _}
import sangria.validation.{BigIntCoercionViolation, IntCoercionViolation}
import sttp.tapir.{Codec, CodecForMany, CodecFormat, CodecMeta, Endpoint, EndpointInfo, EndpointInput, EndpointIO, Schema}
import sttp.tapir.SchemaType.{SInteger, SObjectInfo, SProduct, SString}
import scala.reflect.ClassTag

object EndpointToSangria {

  implicit class RichSangriaEndpoint1[I, E, O](endpoint: Endpoint[I, E, O, _])(implicit iClassTag: ClassTag[I]) {

    def toSangriaField[Ctx, Val](resolve: (Argument[I], Context[Ctx, Val]) => Action[Ctx, O])(
        implicit outType: OutputType[O]
    ): Field[Ctx, Val] = {
      def inputToArgs(input: EndpointInput[_]): List[Argument[_]] = {
        input match {
          case p @ EndpointInput.PathCapture(codec, nameOpt, info) =>
            val name = nameOpt.getOrElse(sys.error("name is required for PathCapture for Sangria generation"))
            Argument(
              name = name,
              argumentType = schemaToSInputType(codec.meta.schema, name),
              description = info.description,
              defaultValue = None,
              fromInput = FromInput.defaultInput[Any],
              astDirectives = Vector.empty,
              astNodes = Vector.empty
            ) :: Nil
          case q @ EndpointInput.Query(name, codec, info) =>
            Argument(
              name = name,
              argumentType = schemaToSInputType(codec.meta.schema, name),
              description = info.description,
              defaultValue = None,
              fromInput = FromInput.defaultInput[Any],
              astDirectives = Vector.empty,
              astNodes = Vector.empty
            ) :: Nil
          case EndpointInput.Cookie(name, codec, info) =>
            Argument(
              name = name,
              argumentType = schemaToSInputType(codec.meta.schema, name),
              description = info.description,
              defaultValue = None,
              fromInput = FromInput.defaultInput[Any],
              astDirectives = Vector.empty,
              astNodes = Vector.empty
            ) :: Nil
          case a: EndpointInput.Auth[I] => inputToArgs(a.input)

          // ? mb use DecodeInputs? Or plainTypeToSArg?
          case e: EndpointInput.ExtractFromRequest[I] => ???
          case EndpointInput.Mapped(wrapped, _, _)    => ???
          case EndpointInput.Multiple(inputs)         =>
            inputs.flatMap(x => inputToArgs(x.asInstanceOf[EndpointInput[_]])).toList
          // case EndpointInput.QueryParams(_)           => ???
          // case EndpointInput.PathsCapture(_)          => ???
          case io: EndpointIO[I] => ??? // ioToArgs(io)

          case _ => List.empty
        }
      }

      // TODO make use of more endpoint docs
      val EndpointInfo(fName, summary, description, tags) = endpoint.info
      val nonEmptyName = fName.getOrElse(sys.error("Endpoint.info.name is required for Sangria Field"))
      val arg = inputToArgs(endpoint.input) match {
        case singleArg :: Nil => singleArg.asInstanceOf[Argument[I]]
        case notSingleArg => sys.error(s"This EndpointToSangria implementation supports exactly 1 arg, got: $notSingleArg")
      }
      Field(nonEmptyName, outType, description, List(arg), resolve(arg, _))
    }


    def schemaToSInputType[T](schema: Schema[T], argName: String): InputType[T] = {
      (schema match {
        case Schema(SString, _, description, _) =>
          ScalarType[String](
            argName,
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
            argName,
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
          val fieldsInpTypes = fieldsS.map { case (fName, fSchema) =>
            fName -> schemaToSInputType(fSchema, s"$argName.$fName") }
          // TODO provide at least descriptions and maybe defaultValues
          val inputFields = fieldsInpTypes.map { case (name, iType) => InputField(name, iType, "") }.toList
          InputObjectType(
            argName,
            description,
            () => inputFields,
            Vector.empty,
            Vector.empty
          )
        // ..
        case unknown => sys.error(s"$unknown input schema match is not implemented")
      }).asInstanceOf[ScalarType[T]]
    }

    def valueOutput[T](value: T, capabilities: Set[MarshallerCapability]): T = value
  }
}
