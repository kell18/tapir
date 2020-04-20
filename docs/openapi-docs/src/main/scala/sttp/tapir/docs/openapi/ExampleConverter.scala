package sttp.tapir.docs.openapi

import sttp.tapir.openapi.OpenAPI.ReferenceOr
import sttp.tapir.{Codec, EndpointIO}
import sttp.tapir.openapi.{Example, ExampleValue}

import scala.collection.immutable.ListMap

private[openapi] object ExampleConverter {
  case class Examples(singleExample: Option[ExampleValue], multipleExamples: ListMap[String, ReferenceOr[Example]])

  def convertExamples[T](o: Codec[_, T, _], examples: List[EndpointIO.Example[T]]): Examples = {
    convertExamples(examples)(exampleValue(o, _))
  }

  private def convertExamples[T](examples: List[EndpointIO.Example[T]])(exampleValue: T => Option[ExampleValue]): Examples = {
    examples match {
      case (example @ EndpointIO.Example(_, None, _)) :: Nil =>
        Examples(exampleValue(example.value), ListMap.empty)
      case examples =>
        val exampleValues = examples.zipWithIndex.map {
          case (example, i) =>
            example.name.getOrElse(s"Example$i") ->
              Right(Example(summary = example.summary, description = None, value = exampleValue(example.value), externalValue = None))
        }.toListMap
        Examples(None, exampleValues)
    }
  }
}
