package tapir.server

import akka.stream.scaladsl.Source
import akka.util.ByteString

package object graphql {
  type AkkaStream = Source[ByteString, Any]
}
