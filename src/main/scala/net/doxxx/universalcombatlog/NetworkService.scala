package net.doxxx.universalcombatlog

import java.net._
import java.nio.charset.Charset
import actors.Actor._
import java.io.{InputStreamReader, BufferedReader, BufferedOutputStream, BufferedInputStream}

/**
 * Created 12-09-25 9:30 AM by gordon.
 */
class NetworkService(port: Int = 5555) {
  import Utils._

  var fights: List[Fight] = Nil

  private val utf8 = Charset.forName("UTF-8")

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
              val outBuf = "ucl://%s:%d/".format(InetAddress.getLocalHost.getHostAddress, port).getBytes(utf8)
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
    val socket = new ServerSocket(port)

    new Thread(new Runnable {
      def run() {
        try {
          while (true) {
            val client = socket.accept()
            log("Accepted connection from client: %s:%d", client.getInetAddress.getHostAddress, client.getPort)
            actor {
              try {
                handleClient(client)
              }
              finally {
                client.close()
              }
            }
          }
        }
        finally {
          socket.close()
        }
      }
    }).start()

    log("Listening for clients")
  }

  private def handleClient(client: Socket) {
    val out = new BufferedOutputStream(client.getOutputStream)

    FileConverter.writeUniversalCombatLog(out, fights)

    out.flush()
    out.close()

    log("Finished writing fights to client: %s:%d", client.getInetAddress.getHostAddress, client.getPort)
  }
}
