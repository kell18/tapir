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

  implicit class RichSangriaEndpoint[I, O, E](endpoint: Endpoint[I, O, E, _])(implicit iClassTag: ClassTag[I]) {
    /*def toSangriaQuery[Ctx](resolver: Resolver[Ctx, Any])(name: String)(
      implicit outType: ObjectType[Ctx, O]
    ): ObjectType[Ctx, _] = {
      val queryField = inputsToQueryField(name)(endpoint, resolver)
      ObjectType(name, fields[Ctx, Any](queryField))
    }*/

    // Query.Field is something like one endpoint - args + resolve method
    def toSangriaField[Ctx, Val, Res](resolve: Context[Ctx, Any] => Action[Ctx, Res])(
        implicit outType: OutputType[O],
        validOutType: ValidOutType[Res, O],
        fromInput: FromInput[I]
    ): Field[Ctx, Any] = {
      @scala.annotation.tailrec
      def inputToArgs(input: EndpointInput[I]): List[Argument[_]] = {
        input match {
          case EndpointInput.FixedMethod(_) => List.empty
          case EndpointInput.FixedPath(_)   => List.empty
          case p @ EndpointInput.PathCapture(codec, name, info) =>
            Argument(name.getOrElse(p.show), schemaToSInputType(codec.meta.schema), info.description.getOrElse("")) :: Nil
          case q @ EndpointInput.Query(name, codec, info) =>
            Argument(name, schemaToSInputType(codec.meta.schema), info.description.getOrElse(name)) :: Nil
          case EndpointInput.Cookie(name, codec, info) =>
            Argument(name, schemaToSInputType(codec.meta.schema), info.description.getOrElse(name)) :: Nil
          case a: EndpointInput.Auth[I] => inputToArgs(a.input)
          // ? mb use DecodeInputs? Or plainTypeToSArg?
          case e: EndpointInput.ExtractFromRequest[I] => ???
          case EndpointInput.Mapped(wrapped, _, _)    => ???
          case EndpointInput.Multiple(inputs)         => ???
          case EndpointInput.QueryParams(_)           => ???
          case EndpointInput.PathsCapture(_)          => ???
          case io: EndpointIO[I]                      => ioToArgs(io)
        }
      }

      def ioToArgs(io: EndpointIO[I]): List[Argument[_]] = {
        io match {
          case EndpointIO.Body(codec, info) =>
            Argument(iClassTag.getClass.getSimpleName, schemaToSInputType(codec.meta.schema), info.description.getOrElse("Endpoint body")) :: Nil
          case _ => ??? // ..
        }
      }

      val EndpointInfo(fName, summary, description, tags) = endpoint.info
      val args = inputToArgs(endpoint.input)
      Field(fName.getOrElse(endpoint.renderPathTemplate()), outType, description, args, resolve)
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
