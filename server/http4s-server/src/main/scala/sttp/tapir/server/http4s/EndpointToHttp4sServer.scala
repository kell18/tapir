package sttp.tapir.server.http4s

import cats.data._
import cats.effect.{ContextShift, Sync}
import cats.implicits._
import com.github.ghik.silencer.silent
import org.http4s.{EntityBody, HttpRoutes, Request, Response}
import org.log4s._
import sttp.tapir.server.internal.{DecodeInputsResult, InputValues, InputValuesResult}
import sttp.tapir.server.{DecodeFailureContext, DecodeFailureHandling, ServerDefaults, ServerEndpoint, internal}
import sttp.tapir.{DecodeResult, Endpoint, EndpointIO, EndpointInput}

import scala.reflect.ClassTag

class EndpointToHttp4sServer[F[_]: Sync: ContextShift](serverOptions: Http4sServerOptions[F]) {
  private val outputToResponse = new OutputToHttp4sResponse[F](serverOptions)

  def toRoutes[I, E, O](se: ServerEndpoint[I, E, O, EntityBody[F], F]): HttpRoutes[F] = {
    val service: HttpRoutes[F] = HttpRoutes[F] { req: Request[F] =>
      def decodeBody(result: DecodeInputsResult): F[DecodeInputsResult] = {
        result match {
          case values: DecodeInputsResult.Values =>
            values.bodyInput match {
              case Some(bodyInput @ EndpointIO.Body(bodyType, codec, _)) =>
                new Http4sRequestToRawBody(serverOptions).apply(req.body, bodyType, req.charset, req).map { v =>
                  codec.decode(v) match {
                    case DecodeResult.Value(bodyV)     => values.setBodyInputValue(bodyV)
                    case failure: DecodeResult.Failure => DecodeInputsResult.Failure(bodyInput, failure): DecodeInputsResult
                  }
                }

              case None => (values: DecodeInputsResult).pure[F]
            }
          case failure: DecodeInputsResult.Failure => (failure: DecodeInputsResult).pure[F]
        }
      }

      def valueToResponse(value: Any): F[Response[F]] = {
        val i = value.asInstanceOf[I]
        se.logic(i)
          .map {
            case Right(result) => outputToResponse(ServerDefaults.StatusCodes.success, se.endpoint.output, result)
            case Left(err)     => outputToResponse(ServerDefaults.StatusCodes.error, se.endpoint.errorOutput, err)
          }
          .flatTap { response => serverOptions.logRequestHandling.requestHandled(se.endpoint, response.status.code) }
          .onError {
            case e: Exception => serverOptions.logRequestHandling.logicException(se.endpoint, e)
          }
      }

      OptionT(decodeBody(internal.DecodeInputs(se.endpoint.input, new Http4sDecodeInputsContext[F](req))).flatMap {
        case values: DecodeInputsResult.Values =>
          InputValues(se.endpoint.input, values) match {
            case hv: InputValuesResult.HasValue            => valueToResponse(hv.value).map(_.some)
            case InputValuesResult.Failure(input, failure) => handleDecodeFailure(se.endpoint, input, failure)
          }
        case DecodeInputsResult.Failure(input, failure) => handleDecodeFailure(se.endpoint, input, failure)
      })
    }

    service
  }

  @silent("never used")
  def toRoutesRecoverErrors[I, E, O](e: Endpoint[I, E, O, EntityBody[F]])(logic: I => F[O])(
      implicit eIsThrowable: E <:< Throwable,
      eClassTag: ClassTag[E]
  ): HttpRoutes[F] = {
    def reifyFailedF(f: F[O]): F[Either[E, O]] = {
      f.map(Right(_): Either[E, O]).recover {
        case e: Throwable if eClassTag.runtimeClass.isInstance(e) => Left(e.asInstanceOf[E]): Either[E, O]
      }
    }

    toRoutes(e.serverLogic(logic.andThen(reifyFailedF)))
  }

  def toRoutes[I, E, O](serverEndpoints: List[ServerEndpoint[_, _, _, EntityBody[F], F]]): HttpRoutes[F] = {
    NonEmptyList.fromList(serverEndpoints.map(se => toRoutes(se))) match {
      case Some(routes) => routes.reduceK
      case None         => HttpRoutes.empty
    }
  }

  private def handleDecodeFailure[I](
      e: Endpoint[_, _, _, _],
      input: EndpointInput[_],
      failure: DecodeResult.Failure
  ): F[Option[Response[F]]] = {
    val decodeFailureCtx = DecodeFailureContext(input, failure)
    val handling = serverOptions.decodeFailureHandler(decodeFailureCtx)
    handling match {
      case DecodeFailureHandling.NoMatch =>
        serverOptions.logRequestHandling.decodeFailureNotHandled(e, decodeFailureCtx).map(_ => None)
      case DecodeFailureHandling.RespondWithResponse(output, value) =>
        serverOptions.logRequestHandling
          .decodeFailureHandled(e, decodeFailureCtx, value)
          .map(_ => Some(outputToResponse(ServerDefaults.StatusCodes.error, output, value)))
    }
  }
}

object EndpointToHttp4sServer {
  private[http4s] val log: Logger = getLogger
}
