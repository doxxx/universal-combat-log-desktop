package net.doxxx.universalcombatlog

import akka.actor._
import spray.can.server._
import spray.http._
import spray.json.DefaultJsonProtocol._
import spray.json._
import java.io._
import java.net._
import java.nio.charset.Charset
import net.doxxx.universalcombatlog.Utils._
import spray.io.{SingletonHandler, IOExtension}

/**
 * Created 12-09-25 9:30 AM by gordon.
 */
class NetworkService(port: Int = 5555) {
  private val system = ActorSystem("NetworkService")
  val ioBridge = IOExtension(system).ioBridge()

  private val utf8 = Charset.forName("UTF-8")

  private val baseURL = new URI("http://%s:%d/".format(InetAddress.getLocalHost.getHostAddress, port))
  log("Base URL: %s", baseURL)

  private var title: String = ""
  private var fights: List[Fight] = Nil

  def setTitleAndFights(_title: String, _fights: List[Fight]) {
    title = _title
    fights = _fights
  }

  def start() {
    listenForBroadcasts()
    listenForClients()
  }

  def listenForBroadcasts() {
    val socket = new DatagramSocket(port)

    new Thread(new Runnable {
      def run() {
        while (true) {
          val buf = new Array[Byte](20)
          val inPacket = new DatagramPacket(buf, buf.length)
          socket.receive(inPacket)
          val message = new String(buf, 0, inPacket.getLength, utf8)
          message match {
            case "UCLDISCOVER" => {
              log("Received discovery datagram from %s:%d", inPacket.getAddress.getHostAddress, inPacket.getPort)
              val outBuf = baseURL.toString.getBytes(utf8)
              val outPacket = new DatagramPacket(outBuf, outBuf.length, inPacket.getSocketAddress)
              socket.send(outPacket)
              log("Sent discovery reply to %s:%d", inPacket.getAddress.getHostAddress, inPacket.getPort)
            }
            case _ => {
              log("Unrecognized datagram contents: %s", message)
            }
          }
        }
        socket.close()
      }
    }).start()

    log("Listening for discovery broadcasts")
  }

  def listenForClients() {
    val clientService = system.actorOf(Props(new ClientService), "client-service")
    val httpServer = system.actorOf((Props(new HttpServer(ioBridge, SingletonHandler(clientService), ServerSettings()))), "http-server")

    httpServer ! HttpServer.Bind("0.0.0.0", port)

    log("Listening for clients")
  }

  class ClientService extends Actor {
    def receive = {
      case HttpRequest(HttpMethods.GET, uri @ "/logfiles", _, _, _) => {
        log("Received GET request for %s", uri)
        val data = List(
          Map("title" -> title, "url" -> baseURL.resolve("logfiles/1").toString)
        ).toJson.prettyPrint
        log("Sending response:\n%s", data)
        sender ! HttpResponse(
          entity = HttpBody(ContentType.`application/json`, data.getBytes(utf8))
        )
      }
      case HttpRequest(HttpMethods.GET, uri @ "/logfiles/1", _, _, _) => {
        log("Received GET request for %s", uri)
        val data = new ByteArrayOutputStream()
        FileConverter.writeUniversalCombatLog(data, fights)
        sender ! HttpResponse(
          entity = HttpBody(ContentType.`application/octet-stream`, data.toByteArray)
        )
        log("Sent fights in response")
      }
      case HttpRequest(_, _, _, _, _) => {
        sender ! HttpResponse(StatusCodes.NotFound)
      }
    }
  }
}
