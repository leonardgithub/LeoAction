package leo.action

import scala.beans.BeanProperty

trait IImInfo {

  def setNum(num: String)

  def getNum(): String

  def setIsLogin(isLogin: Boolean)

  def getIsLogin(): Boolean

  def setSocketUuid(uuid: String)

  def getSocketUuid(): String

  def getLocalStorageSupported: Boolean

  def setLocalStorageSupported(f: Boolean)

  //  def setIsOpened(num: Boolean)
  //  def getIsOpened(): Boolean
}

class ImInfo extends IImInfo {

  @BeanProperty var num: String = null

  @BeanProperty var isLogin: Boolean = false

  @BeanProperty var socketUuid: String = ""

  @BeanProperty var localStorageSupported: Boolean = false

  override def toString(): String = {
    "(num:" + num + ", socket uuid:" + socketUuid + ", isLogin:" + isLogin + ")"
  }

}