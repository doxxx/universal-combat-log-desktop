package net.doxxx.universalcombatlog

import java.net._
import java.nio.charset.Charset
import java.io._
import akka.actor.{Actor, Supervisor}
import akka.config.Supervision.{Permanent, Supervise, OneForOneStrategy, SupervisorConfig}
import cc.spray.can._
import cc.spray.json._
import DefaultJsonProtocol._
import Utils._

/**
 * Created 12-09-25 9:30 AM by gordon.
 */
class NetworkService(port: Int = 5555) {
  private val utf8 = Charset.forName("UTF-8")
  private val serverConfig = ServerConfig(
    host = "0.0.0.0",
    port = port
  )
  private val baseURL = "http://%s:%d/".format(InetAddress.getLocalHost.getHostAddress, port)
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
              val outBuf = baseURL.getBytes(utf8)
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
    Supervisor(
      SupervisorConfig(
        OneForOneStrategy(List(classOf[Exception]), 3, 100),
        List(
          Supervise(Actor.actorOf(new ClientService()), Permanent),
          Supervise(Actor.actorOf(new HttpServer(serverConfig)), Permanent)
        )
      )
    )

    log("Listening for clients")
  }

  class ClientService extends Actor {
    self.id = "spray-root-service"
    protected def receive = {
      case RequestContext(HttpRequest(HttpMethods.GET, "/logfiles", _, _, _), _, responder) => {
        log("Received GET request for /logfiles")
        val data = List(
          Map("title" -> title, "url" -> (baseURL + "logfiles/1"))
        ).toJson.prettyPrint
        log("Sending response:\n%s", data)
        responder.complete(HttpResponse(
          headers = List(HttpHeader("Content-Type", "application/json")),
          body = data.getBytes(utf8)
        ))
      }
      case RequestContext(HttpRequest(HttpMethods.GET, "/logfiles/1", _, _, _), _, responder) => {
        log("Received GET request for /logfiles/1")
        val data = new ByteArrayOutputStream()
        FileConverter.writeUniversalCombatLog(data, fights)
        responder.complete(HttpResponse(body = data.toByteArray))
        log("Sent fights in response")
      }
      case RequestContext(HttpRequest(_, _, _, _, _), _, responder) => {
        responder.complete(HttpResponse(status=404))
      }
    }
  }
}
