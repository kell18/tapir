package tapir.docs.openapi.schema

import tapir.openapi.OpenAPI.ReferenceOr
import tapir.openapi.{SchemaType, Schema => OSchema}
import tapir.{Codec, CodecForMany, CodecForOptional, Schema => TSchema, SchemaType => TSchemaType}

class ObjectSchemas(
    tschemaToOSchema: TSchemaToOSchema,
    schemaReferenceMapper: SchemaReferenceMapper
) {
  def apply[T](codec: CodecForMany[T, _, _]): ReferenceOr[OSchema] = apply(TypeData(codec))
  def apply[T](codec: Codec[T, _, _]): ReferenceOr[OSchema] = apply(TypeData(codec))
  def apply[T](codec: CodecForOptional[T, _, _]): ReferenceOr[OSchema] = apply(TypeData(codec))

  def apply(typeData: TypeData[_, _]): ReferenceOr[OSchema] = {
    typeData.schemaType match {
      case TSchemaType.SArray(TSchema(o: TSchemaType.SObject, _)) =>
        Right(OSchema(SchemaType.Array).copy(items = Some(Left(schemaReferenceMapper.map(o.info)))))
      case o: TSchemaType.SObject => Left(schemaReferenceMapper.map(o.info))
      case _                      => tschemaToOSchema(typeData)
    }
  }
}
