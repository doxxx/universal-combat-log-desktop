package net.doxxx.universalcombatlog

import java.net._
import java.nio.charset.Charset
import java.io._
import akka.actor.{Actor, Supervisor}
import akka.config.Supervision.{Permanent, Supervise, OneForOneStrategy, SupervisorConfig}
import cc.spray.can._

/**
 * Created 12-09-25 9:30 AM by gordon.
 */
class NetworkService(port: Int = 5555) {
  import Utils._

  var fights: List[Fight] = Nil

  private val utf8 = Charset.forName("UTF-8")
  private val serverConfig = ServerConfig(
    host = "0.0.0.0",
    port = port
  )

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
              val outBuf = "http://%s:%d/".format(InetAddress.getLocalHost.getHostAddress, port).getBytes(utf8)
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
      case RequestContext(HttpRequest(HttpMethods.GET, "/", _, _, _), _, responder) => {
        log("Received GET request for /")
        val data = new ByteArrayOutputStream();
        FileConverter.writeUniversalCombatLog(data, fights)
        responder.complete(HttpResponse(body = data.toByteArray))
        log("Sent fights in response")
      }
    }
  }
}
