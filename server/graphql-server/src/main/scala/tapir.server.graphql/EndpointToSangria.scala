package tapir.server.graphql

import sangria.ast
import sangria.marshalling.{FromInput, MarshallerCapability}
import sangria.schema.{Schema => SangriaSchema, _}
import sangria.validation.{BigIntCoercionViolation, IntCoercionViolation}
import sttp.tapir.{Codec, CodecForMany, CodecFormat, CodecMeta, Endpoint, EndpointInfo, EndpointInput, EndpointIO, Schema}
import sttp.tapir.SchemaType.{SInteger, SObjectInfo, SProduct, SString}
import scala.reflect.ClassTag

object EndpointToSangria {
  type Resolver[Ctx, Val] = Context[Ctx, Val] ⇒ Action[Ctx, _]

  implicit class RichSangriaEndpoint[I, E, O](endpoint: Endpoint[I, E, O, _])(implicit iClassTag: ClassTag[I]) {
    /*def toSangriaQuery[Ctx](resolver: Resolver[Ctx, Any])(name: String)(
      implicit outType: ObjectType[Ctx, O]
    ): ObjectType[Ctx, _] = {
      val queryField = inputsToQueryField(name)(endpoint, resolver)
      ObjectType(name, fields[Ctx, Any](queryField))
    }*/

    // Query.Field is something like one endpoint - args + resolve method
    def toSangriaField[Ctx, Val](resolve: (Argument[I], Context[Ctx, Val]) => Action[Ctx, O])(
        implicit outType: OutputType[O]
    ): Field[Ctx, Val] = {
      def inputToArgs(input: EndpointInput[_]): List[Argument[_]] = {
        input match {
          case p @ EndpointInput.PathCapture(codec, name, info) =>
            Argument(
              name = name.getOrElse(p.show),
              argumentType = schemaToSInputType(codec.meta.schema),
              description = info.description,
              defaultValue = None,
              fromInput = FromInput.defaultInput[Any],
              astDirectives = Vector.empty,
              astNodes = Vector.empty
            ) :: Nil
          case q @ EndpointInput.Query(name, codec, info) =>
            Argument(
              name = name,
              argumentType = schemaToSInputType(codec.meta.schema),
              description = info.description,
              defaultValue = None,
              fromInput = FromInput.defaultInput[Any],
              astDirectives = Vector.empty,
              astNodes = Vector.empty
            ) :: Nil
          case EndpointInput.Cookie(name, codec, info) =>
            Argument(
              name = name,
              argumentType = schemaToSInputType(codec.meta.schema),
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

      /*def ioToArgs(io: EndpointIO[I]): List[Argument[_]] = {
        io match {
          case EndpointIO.Body(codec, info) =>
            Argument(iClassTag.getClass.getSimpleName, schemaToSInputType(codec.meta.schema), info.description.getOrElse("Endpoint body")) :: Nil
          case _ => ??? // ..
        }
      }*/

      val EndpointInfo(fName, summary, description, tags) = endpoint.info
      val nonEmptyName = fName.getOrElse(sys.error("Endpoint.info.name is required for Sangria Field"))
      val arg = inputToArgs(endpoint.input).headOption
        .getOrElse(sys.error("This implementation supports only 1 arg"))
        .asInstanceOf[Argument[I]]
      Field(nonEmptyName, outType, description, List(arg), resolve(arg, _))
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
  }
}
