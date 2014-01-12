package leo.action

trait IImAppConnection {

  def sendMessage(data: String): Boolean

  def isOpen(): Boolean

  def onClose(code: Int, msg: String): Unit
  def onTextMessage(msg: String): Unit

  def setMaxIdleTime(i: Int): Unit
  def setMaxBinaryMessageSize(i: Int): Unit
  def setMaxTextMessageSize(i: Int): Unit

}