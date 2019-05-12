package tapir
import tapir.model.Method

package object internal {
  implicit class RichEndpointInput[I](input: EndpointInput[I]) {
    def asVectorOfSingleInputs: Vector[EndpointInput.Single[_]] = input match {
      case s: EndpointInput.Single[_]   => Vector(s)
      case m: EndpointInput.Multiple[_] => m.inputs
      case m: EndpointIO.Multiple[_]    => m.ios
    }

    def traverseInputs[T](handle: PartialFunction[EndpointInput[_], Vector[T]]): Vector[T] = input match {
      case i: EndpointInput[_] if handle.isDefinedAt(i) => handle(i)
      case EndpointInput.Multiple(inputs)               => inputs.flatMap(_.traverseInputs(handle))
      case EndpointIO.Multiple(inputs)                  => inputs.flatMap(_.traverseInputs(handle))
      case EndpointInput.Mapped(wrapped, _, _, _)       => wrapped.traverseInputs(handle)
      case EndpointIO.Mapped(wrapped, _, _, _)          => wrapped.traverseInputs(handle)
      case a: EndpointInput.Auth[_]                     => a.input.traverseInputs(handle)
      case _                                            => Vector.empty
    }

    def asVectorOfBasicInputs(includeAuth: Boolean = true): Vector[EndpointInput.Basic[_]] = traverseInputs {
      case b: EndpointInput.Basic[_] => Vector(b)
      case a: EndpointInput.Auth[_]  => if (includeAuth) a.input.asVectorOfBasicInputs(includeAuth) else Vector.empty
    }

    def auths: Vector[EndpointInput.Auth[_]] = traverseInputs {
      case a: EndpointInput.Auth[_] => Vector(a)
    }

    def method: Option[Method] =
      traverseInputs {
        case i: EndpointInput.RequestMethod => Vector(i.m)
      }.headOption
  }

  implicit class RichBasicEndpointInputs(inputs: Vector[EndpointInput.Basic[_]]) {
    def sortByType: Vector[EndpointInput.Basic[_]] = inputs.sortBy {
      case _: EndpointInput.RequestMethod         => 0
      case _: EndpointInput.PathSegment           => 1
      case _: EndpointInput.PathCapture[_]        => 1
      case _: EndpointInput.PathsCapture          => 1
      case _: EndpointInput.Query[_]              => 2
      case _: EndpointInput.QueryParams           => 2
      case _: EndpointInput.Cookie[_]             => 3
      case _: EndpointIO.Header[_]                => 3
      case _: EndpointIO.Headers                  => 3
      case _: EndpointInput.ExtractFromRequest[_] => 4
      case _: EndpointIO.Body[_, _, _]            => 6
      case _: EndpointIO.StreamBodyWrapper[_, _]  => 6
    }
  }

  implicit class RichEndpointOutput[I](output: EndpointOutput[I]) {
    def asVectorOfSingleOutputs: Vector[EndpointOutput.Single[_]] = output match {
      case s: EndpointOutput.Single[_]   => Vector(s)
      case m: EndpointOutput.Multiple[_] => m.outputs
      case m: EndpointIO.Multiple[_]     => m.ios
    }

    def traverseOutputs[T](handle: PartialFunction[EndpointOutput[_], Vector[T]]): Vector[T] = output match {
      case o: EndpointOutput[_] if handle.isDefinedAt(o) => handle(o)
      case EndpointOutput.Multiple(outputs)              => outputs.flatMap(_.traverseOutputs(handle))
      case EndpointIO.Multiple(outputs)                  => outputs.flatMap(_.traverseOutputs(handle))
      case EndpointOutput.Mapped(wrapped, _, _, _)       => wrapped.traverseOutputs(handle)
      case EndpointIO.Mapped(wrapped, _, _, _)           => wrapped.traverseOutputs(handle)
      case s: EndpointOutput.StatusFrom[_]               => s.output.traverseOutputs(handle)
      case _                                             => Vector.empty
    }

    def asVectorOfBasicOutputs: Vector[EndpointOutput.Basic[_]] = traverseOutputs {
      case b: EndpointOutput.Basic[_] => Vector(b)
    }

    def bodyType: Option[RawValueType[_]] =
      traverseOutputs[RawValueType[_]] {
        case b: EndpointIO.Body[_, _, _] => Vector(b.codec.meta.rawValueType)
      }.headOption
  }

  implicit class RichBasicEndpointOutputs(outputs: Vector[EndpointOutput.Basic[_]]) {
    def sortByType: Vector[EndpointOutput.Basic[_]] = outputs.sortBy {
      case _: EndpointOutput.StatusCode          => 0
      case _: EndpointIO.Header[_]               => 1
      case _: EndpointIO.Headers                 => 1
      case _: EndpointIO.Body[_, _, _]           => 2
      case _: EndpointIO.StreamBodyWrapper[_, _] => 2
    }
  }
}
