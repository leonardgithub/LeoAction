package leo.action

import scala.beans.BeanProperty
import scala.collection.mutable

import leo.util.LeoLogger

object LeoSocketManager {
  val loginSocketsLock = "loginSocketsLock"
  val rawSocketsLock = "rawSocketsLock"
  val channelLock = "channelLock"

  @BeanProperty var loginSockets: mutable.Map[String, mutable.Set[IImSocket]] = new mutable.HashMap[String, mutable.Set[IImSocket]]()

  @BeanProperty var rawSockets: mutable.Set[IImSocket] = new mutable.HashSet[IImSocket]()

  // just for SAE channel
  @BeanProperty var channels: mutable.Map[String, IImSocket] = new mutable.HashMap[String, IImSocket]()

  def doWithLoginSocketsLock(f: () => Unit) {
    synchronized[String](loginSocketsLock) {
      f()
      0
    }
  }

  def doWithRawSocketsLock(f: () => Unit) {
    synchronized[String](rawSocketsLock) {
      f()
      0
    }
  }
  def doWithChannelLock(f: () => Unit) {
    synchronized[String](channelLock) {
      f()
      0
    }
  }
  def addChannel(id: String, channel: IImSocket) {
    doWithChannelLock(() => {
      channels.put(id, channel)
    })
  }
  def removeChannel(id: String) {
    doWithChannelLock(() => {
      channels.remove(id)
    })
  }
  def getChannel(id: String): IImSocket = {
    channels.getOrElse(id, null)
  }
  def addRawSocket(socket: IImSocket) {
    doWithRawSocketsLock(() => {
      rawSockets.add(socket)
    })
  }
  def removeRawSocket(socket: IImSocket) {
    doWithRawSocketsLock(() => {
      rawSockets.remove(socket)
    })
  }
  def addLoginSocket(key: String, socket: IImSocket) {
    doWithLoginSocketsLock(() => {
      if (loginSockets.contains(key)) {
        val sockets = loginSockets(key)
        sockets.add(socket)
      } else {
        val sockets = new mutable.HashSet[IImSocket]()
        sockets.add(socket)
        loginSockets.put(key, sockets)
      }
    })
  }
  def getLoginSockets(num: String): mutable.Set[IImSocket] = {
    loginSockets.getOrElse(num, null)
  }
  def checkIsLoginByNum(num: String) = {
    getLoginSockets(num) match {
      case null => false
      case s => !s.isEmpty
    }
  }
  def getLoginSocketsSize(): Int = {
    if (loginSockets == null) 0 else loginSockets.size
  }
  def getRawSocketsSize(): Int = {
    if (rawSockets == null) 0 else rawSockets.size
  }
  def removeLoginSocket(key: String, socket: IImSocket): Unit = {
    doWithLoginSocketsLock(() => {
      LeoLogger.log("---removeLoginSocket begin")
      LeoLogger.log("key " + key)
      if (loginSockets.contains(key)) {
        LeoLogger.log("contain key " + key)
        val sockets = loginSockets(key)
        LeoLogger.log("---sockets.size before remove" + sockets.size)
        sockets.remove(socket)
        if (sockets.isEmpty) {
          loginSockets.remove(key)
        }
        LeoLogger.log("---sockets.size after remove" + sockets.size)
      }
      LeoLogger.log("---removeLoginSocket end")
    })
  }
  def switchToLogin(key: String, socket: IImSocket): Unit = {
    doWithLoginSocketsLock(() => {
      doWithRawSocketsLock(() => {
        if (rawSockets.contains(socket)) {
          addLoginSocket(key, socket)
          rawSockets.remove(socket)
        }
      })
    })
  }
  def doInLoginSockets(num: String, f: (IImSocket) => Unit) {
    val sockets = LeoSocketManager.getLoginSockets(num)
    sockets match {
      case null => //do nothing
      case _ =>
        sockets.foreach(
          //one callback start
          toSocket =>
            toSocket match {
              case null => //do nothing
              case _ =>
                f(toSocket)
            } //one callback end
            ) //foreach end
    } //match ends
  }
  def getInfo(): String = {
    "loginSockets.size:" + this.loginSockets.size + ",channels.size:" + this.channels.size + "rawSockets.size:" + this.rawSockets.size
  }
}