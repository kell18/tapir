package tapir.server.graphql

import sangria.ast
import sangria.marshalling.{FromInput, MarshallerCapability}
import sangria.schema.{Schema => SangriaSchema, _}
import sangria.validation.{BigIntCoercionViolation, IntCoercionViolation}
import sttp.tapir.{Codec, CodecFormat, Endpoint, EndpointInfo, EndpointInput, EndpointIO, Schema}
import sttp.tapir.SchemaType.{SInteger, SObjectInfo, SProduct, SString}
import scala.reflect.ClassTag

object EndpointToSangria {

  implicit class RichSangriaEndpoint[I, E, O](endpoint: Endpoint[I, E, O, _]) {

    // TODO think how to generilize on N arguments
    def toSangriaField[Ctx, Val](resolve: (Argument[I], Context[Ctx, Val]) => Action[Ctx, O])(
        implicit outType: OutputType[O]
    ): Field[Ctx, Val] = {
      val EndpointInfo(fName, summary, description, tags, deprecated) = endpoint.info
      val nonEmptyName = fName.getOrElse(sys.error("Endpoint.info.name is required for Sangria Field"))
      val arg = inputToArgs(endpoint.input) match {
        case singleArg :: Nil => singleArg.asInstanceOf[Argument[I]]
        case notSingleArg     => sys.error(s"This EndpointToSangria implementation supports exactly 1 arg, got: $notSingleArg")
      }
      Field(nonEmptyName, outType, description, List(arg), resolve(arg, _))
    }

    // TODO Account codecs validators
    def inputToArgs(input: EndpointInput[_]): List[Argument[_]] = {
      input match {
        case p @ EndpointInput.PathCapture(nameOpt, codec, info) =>
          val name = nameOpt.getOrElse(sys.error("name is required for PathCapture for Sangria generation"))
          Argument(
            name = name,
            argumentType = schemaToSInputType(codec.schema.get, name),
            description = info.description,
            defaultValue = None,
            fromInput = FromInput.defaultInput[Any],
            astDirectives = Vector.empty,
            astNodes = Vector.empty
          ) :: Nil
        case q @ EndpointInput.Query(name, codec, info) =>
          Argument(
            name = name,
            argumentType = schemaToSInputType(codec.schema.get, name),
            description = info.description,
            defaultValue = None,
            fromInput = FromInput.defaultInput[Any],
            astDirectives = Vector.empty,
            astNodes = Vector.empty
          ) :: Nil
        case EndpointInput.Cookie(name, codec, info) =>
          Argument(
            name = name,
            argumentType = schemaToSInputType(codec.schema.get, name),
            description = info.description,
            defaultValue = None,
            fromInput = FromInput.defaultInput[Any],
            astDirectives = Vector.empty,
            astNodes = Vector.empty
          ) :: Nil
        case a: EndpointInput.Auth[I] => inputToArgs(a.input)
        case EndpointInput.Multiple(inputs, _, _) =>
          inputs.flatMap(x => inputToArgs(x.asInstanceOf[EndpointInput[_]])).toList

        case _ => List.empty // TODO Mapped?, ExtractFromRequest?, QueryParams, PathsCapture, EndpointIO
      }
    }

    def schemaToSInputType[T](schema: Schema[T], argName: String): InputType[T] = {
      schema match {
        case Schema(SString, _, description, _, _) =>
          ScalarType[String](
            argName,
            description,
            coerceUserInput = { any =>
              Right(any.toString)
            },
            coerceInput = { v =>
              Right(v.renderPretty)
            },
            // TODO make sure it's correct
            coerceOutput = { case (t, _) => t.toString }
          ).asInstanceOf[ScalarType[T]]
        case Schema(SInteger, _, description, _, _) =>
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
            coerceOutput = (value: Int, _: Set[MarshallerCapability]) => value
          ).asInstanceOf[ScalarType[T]]
        case Schema(SProduct(SObjectInfo(fullName, tParamsNames), fieldsS), isOptional, description, format, _) =>
          val fieldsInpTypes = fieldsS.map {
            case (fName, fSchema) =>
              val name = s"$argName.$fName"
              val fType = schemaToSInputType(fSchema, name)
              val descr = fSchema.description
                .getOrElse(sys.error(s"Description is required for object fields ($name)"))
              // TODO provide defaultValues if possible
              InputField(fName, fType, descr)
          }
          InputObjectType(
            argName,
            description,
            () => fieldsInpTypes.toList,
            Vector.empty,
            Vector.empty
          )
        case _ => ???
      }
    }

    def valueOutput[T](value: T, capabilities: Set[MarshallerCapability]): T = value
  }
}
