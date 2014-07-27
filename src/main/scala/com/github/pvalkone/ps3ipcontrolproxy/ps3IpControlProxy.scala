package com.github.pvalkone.ps3ipcontrolproxy

import java.lang.Thread.sleep
import java.net.HttpURLConnection._
import java.net.InetSocketAddress
import java.util.concurrent.Executors

import com.github.pvalkone.ps3ipcontrolproxy.SixaxisControllerKey.SixaxisControllerKey
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import scala.sys.exit
import scala.sys.process._
import scala.util.{Failure, Success, Try}

/*
 * A hacky proxy server for allowing IP control of a PlayStation 3
 * console over a simple HTTP API.
 *
 * Probably not very useful for other than home theatre integration
 * using appropriate remote control software.
 *
 * Requires Linux, Oracle Java 7, GIMX (Game Input MultipleXer)
 * and a Bluetooth adapter. Tested on Raspbian Wheezy.
 *
 * To run: $ ./sbt "run F0:F0:02:xx:xx:xx"
 */
object Ps3IpControlProxyServer {
  def main(args: Array[String]): Unit = {
    val (ps3BluetoothDeviceAddress, port) = parseCommandLineArguments(args)
    val server = HttpServer.create(new InetSocketAddress(port), 0)
    server.createContext("/", new Ps3IpControlHandler(ps3BluetoothDeviceAddress))
    server.setExecutor(Executors.newCachedThreadPool())
    server.start()
    println("Listening on port %d, connected to PS3 at %s".format(port, ps3BluetoothDeviceAddress))
  }

  private def parseCommandLineArguments(args: Array[String]) = {
    val defaultPort = 9090
    if (args.length < 1 || args.length > 2) {
      println(
        """usage: %s ps3_bd_address [port]
          |  ps3_bd_address: Bluetooth Device Address of the PS3 to connect to
          |  port: Port to bind the proxy server to (default: %d)"""
          .stripMargin.format(getClass.getName, defaultPort))
      exit(1)
    }
    val ps3BluetoothDeviceAddress = args(0)
    val port = if (args.length == 2) Integer.parseInt(args(1)) else defaultPort
    (ps3BluetoothDeviceAddress, port)
  }
}

class Ps3IpControlHandler(ps3BluetoothDeviceAddress: String) extends HttpHandler {
  private val processLogger = ProcessLogger(line => ())

  override def handle(exchange: HttpExchange) {
    val status = Try {
      exchange.getRequestURI.getPath match {
        case "/ps3/power/on" => powerOn()
        case "/ps3/power/off" => powerOff()
        case "/ps3/power/toggle" => if (gimxServerStarted) powerOff() else powerOn()
        case "/ps3/key/select" => sendKeyPress(SixaxisControllerKey.SELECT)
        case "/ps3/key/start" => sendKeyPress(SixaxisControllerKey.START)
        case "/ps3/key/ps" => sendKeyPress(SixaxisControllerKey.PS)
        case "/ps3/key/up" => sendKeyPress(SixaxisControllerKey.UP)
        case "/ps3/key/right" => sendKeyPress(SixaxisControllerKey.RIGHT)
        case "/ps3/key/down" => sendKeyPress(SixaxisControllerKey.DOWN)
        case "/ps3/key/left" => sendKeyPress(SixaxisControllerKey.LEFT)
        case "/ps3/key/triangle" => sendKeyPress(SixaxisControllerKey.TRIANGLE)
        case "/ps3/key/circle" => sendKeyPress(SixaxisControllerKey.CIRCLE)
        case "/ps3/key/cross" => sendKeyPress(SixaxisControllerKey.CROSS)
        case "/ps3/key/square" => sendKeyPress(SixaxisControllerKey.SQUARE)
        case "/ps3/key/l1" => sendKeyPress(SixaxisControllerKey.L1)
        case "/ps3/key/r1" => sendKeyPress(SixaxisControllerKey.R1)
        case "/ps3/key/l2" => sendKeyPress(SixaxisControllerKey.L2)
        case "/ps3/key/r2" => sendKeyPress(SixaxisControllerKey.R2)
        case "/ps3/key/l3" => sendKeyPress(SixaxisControllerKey.L3)
        case "/ps3/key/r3" => sendKeyPress(SixaxisControllerKey.R3)
        case action => throw new UnsupportedOperationException(s"Unknown action $action")
      }
    } match {
      case Failure(e: UnsupportedOperationException) => HTTP_NOT_FOUND
      case Failure(e) => {
        e.printStackTrace()
        HTTP_INTERNAL_ERROR
      }
      case Success(_) => HTTP_NO_CONTENT
    }
    exchange.sendResponseHeaders(status, -1)
    exchange.close()
  }

  private def gimxServerStarted = ("/usr/bin/pgrep gimx" !) == 0

  private def powerOn() {
    if (!gimxServerStarted) {
      startGimxServer(ps3BluetoothDeviceAddress)
    }
  }

  private def powerOff() {
    sendPowerOffSequence()
  }

  private def startGimxServer(ps3BluetoothDeviceAddress: String) {
    if (gimxServerStarted) {
      return
    }
    // Start a GIMX server in the background, bringing the PS3 out of standby
    s"/usr/bin/gimx --type Sixaxis --src 127.0.0.1:51914 --bdaddr $ps3BluetoothDeviceAddress".run(processLogger)
    // Wait for the PS3 to finish booting and then activate the controller by pressing the PS button
    sendDelayedKeyPress(key = SixaxisControllerKey.PS, delay = 35)
  }

  private def sendPowerOffSequence() {
    sendKeyPress(SixaxisControllerKey.PS, 3.0)
    sendKeyPress(SixaxisControllerKey.CROSS)
    sleep(500)
    sendKeyPress(SixaxisControllerKey.CROSS)
  }

  private def sendKeyPress(key: SixaxisControllerKey, duration: Double = 0.1) {
    buildKeyPress(key, duration) ! processLogger
  }

  private def sendDelayedKeyPress(key: SixaxisControllerKey, duration: Double = 0.1, delay: Double) {
    (s"sleep $delay" #&& buildKeyPress(key, duration)).run(processLogger)
  }

  private def buildKeyPress(key: SixaxisControllerKey, duration: Double) = {
    (
      Seq("/usr/bin/gimx", "--dst", "127.0.0.1:51914", "--event", s"$key(255)") #&& // Key down
      s"sleep $duration" #&&
      Seq("/usr/bin/gimx", "--dst", "127.0.0.1:51914", "--event", s"$key(0)") // Key up
    )
  }
}

object SixaxisControllerKey extends Enumeration {
  type SixaxisControllerKey = Value

  val SELECT = Value("select")
  val START = Value("start")
  val PS = Value("PS")
  val UP = Value("up")
  val RIGHT = Value("right")
  val DOWN = Value("down")
  val LEFT = Value("left")
  val TRIANGLE = Value("triangle")
  val CIRCLE = Value("circle")
  val CROSS = Value("cross")
  val SQUARE = Value("square")
  val L1 = Value("l1")
  val R1 = Value("r1")
  val L2 = Value("l2")
  val R2 = Value("r2")
  val L3 = Value("l3")
  val R3 = Value("r3")
}
