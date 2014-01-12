package leo.action

import scala.collection.immutable
import leo.biz._
import leo.entity._
import leo.util.LeoLogger
import leo.web.uitl.ParamMap
import java.util.Date
import leo.util.LeoUtil
import java.io.FileInputStream
import java.io.File
import java.io.FileOutputStream

trait ILeoSocketFactory {
  def createILeoSocket(): IImSocket
}

class ImActionController {
}

object ImActionController {

  def main(args: Array[String]) {
    val ret = "" match {
      case "1" => "lx"
      case _ => "lx_"
    }
    println(ret)
  }

  // only for tomcat/jetty websocket, not for SAE channel
  def doWebSocketConnect(fac: ILeoSocketFactory, protocol: String, connectType: String, num: String, pwd: String): IImSocket = {
    LeoLogger.log("ActionController.doWebSocketConnect")
    LeoLogger.log("connectType: " + connectType)
    connectType match {
      case null =>
        return null
      case "connectBeforeLogin" =>
        return fac.createILeoSocket
      case "login" =>
        // TODO: note: seems this case is deprecated
        if (num == null || "".equals(num) || pwd == null || "".equals(pwd)) {
          return null
        }
        val user = new ImUser()
        user.setNum(num)
        user.setPwd(pwd)
        val ret = ImUserBiz.loginUser(user)
        return DtoHelper.doForDto[ImUser, IImSocket](
          ret,
          () => {
            val s: IImSocket = fac.createILeoSocket
            s.setNum(num)
            s.setIsLogin(true)
            s
          },
          null)
      case "reconnect" =>
        var s: IImSocket = fac.createILeoSocket
        if (false) {
          //note: seems deprecated, because it need send num/pwd to server after js onopen
          s.setNum(num)
          s.setIsLogin(true)
        }
        return s
      case _ => return null
    } //match ends
  }
  def doOnMessage(data: String, socket: IImSocket): Unit = {
    LeoLogger.log("ActionController.doOnMessage start")
    val paramMap = ParamMap(data)
    var sysParamMap = paramMap.getSysParamMap()
    var appParamMap = paramMap.getAppParamMap()
    if (sysParamMap.contains(ParamMap.action) && sysParamMap.contains(ParamMap.method)) {
      val action = sysParamMap(ParamMap.action)
      val method = sysParamMap(ParamMap.method)
      val m = ImActionMap.getHandler(action, method)
      if (m == null) {
        LeoLogger.log("no action or method")
      } else {
        // invoke method
        m(paramMap, socket)
      }
    } else { //invalid parameters
      LeoLogger.log("no action or method")
    }
  }
}

object ImActionMap {
  val leoMainMsg = "leoMainMsg"
  val imFriend = "imFriend"

  val actionMap: immutable.Map[String, immutable.Map[String, (ParamMap, IImSocket) => Unit]] = immutable.Map(
    leoMainMsg -> immutable.Map(
      "sendMsg" -> ImMsgAction.sendMsg,
      "sendImg" -> ImMsgAction.sendImg,
      "login" -> ImMsgAction.doLogin,
      "getMsgsByUuids" -> ImMsgAction.getMsgsByUuids,
      "getServerUnreadImMsgs" -> ImMsgAction.getServerUnreadImMsgs,
      "getUserByCondition" -> ImMsgAction.getUserByCondition,
      "addFri" -> ImMsgAction.addFriend,
      //      "getLatestQqMsg" -> LeoMainMsg.getLatestLeoQqMsg,
      "getLists" -> ImMsgAction.getLeoQqListInList),

    imFriend -> immutable.Map("agreeAddFriend" -> ImFriendAction.agreeAddFriend),
    "xxx5" -> immutable.Map("xxx" -> null),
    "xxx5" -> immutable.Map("xxx" -> null))

  def getHandler(action: String, method: String) = {
    if (actionMap.contains(action)) {
      val m = actionMap(action)
      if (m.contains(method)) {
        m(method) // return method/action
      } else {
        null
      }
    } else {
      null
    }
  }
}

//object LeoQqMsgAction {
//
//}
//object LoginAction {
//
//}

//object FriendAction {
//  def findFriend(pm: ParamMap, s: IImSocket) {
//
//  }
//}

//object ActionHelper {
//
//  def doByDto[T](dto: LeoDTO[T], success: () => Unit, fail: () => Unit) {
//    dto match {
//      case null => if (fail != null) { fail() }
//      case _ =>
//        dto.getIsSuccess match {
//          case false => if (fail != null) { fail() }
//          case true => if (success != null) { success() }
//        }
//    }
//  }
//
//  def main(args: Array[String]) {
//    doByDto(null, () => {
//      println("success")
//    }, () => {
//      println("f")
//    })
//  }
//}

object ImMsgAction {
  def getLeoQqListInList(pm: ParamMap, s: IImSocket) {
    //    var appParamMap = pm.getAppParamMap()
    val num = s.getNum
    val dto = ImFriendGroupBiz.getImFriendGroups(num)
    DtoHelper.doForDto(dto, () => {
      pm.setAppParamMap(immutable.Map("lists" -> dto.getEntity))
      s.sendMessage(ParamMap.stringify(pm))
    }, null)
  }

  def getMsgsByUuids(pm: ParamMap, s: IImSocket) {
    //    var appParamMap = pm.getAppParamMap()
    //    val msgUuids = appParamMap.getOrElse("msgUuids", "")
    //    msgUuids match {
    //      case msgUuidList: List[String] => //invoke biz
    //    }
  }

  def addFriend(pm: ParamMap, s: IImSocket) {
    var app = pm.getAppParamMap()
    val to = app.getOrElse("to", "")
    val c = app.getOrElse("c", "")

    LeoUtil.convertToString(to) match {
      case "" =>
      case _ =>
        val e = new ImAddFriendMsg()
        e.setToUserNum(to.toString)
        e.setContent(LeoUtil.convertToString(c).trim())
        e.setFromUserNum(s.getNum)
        e.setCreationTime(LeoUtil.getCurTime)

        sendMsgTemplate(s.getNum, to.toString,
          (isSentToReceiver: Boolean) => {
            // save to DB
            e.setIsRead(if (isSentToReceiver) "1" else "0")
            ImAddFriendBiz.insert(e)
          },
          false,
          (_socket: IImSocket) => {
            //  successful response to sender
            val responsePm = ParamMap()
            responsePm.setSysParamMap(pm.getSysParamMap)
            responsePm.setAppParamMap(immutable.Map("ret" -> "1"))
            s.sendMessage(ParamMap.stringify(responsePm)) //use s instead of _socket
          },
          (_socket: IImSocket) => {
            //  response to receiver
            val responsePm = ParamMap(ImActionMap.leoMainMsg, "getAddFriMsg")
            responsePm.setAppParamMap(immutable.Map("o" -> e)) //u = user
            _socket.sendMessage(ParamMap.stringify(responsePm))
          },
          (_socket: IImSocket) => {
            //  failed response to sender
            pm.setAppParamMap(immutable.Map("ret" -> "0"))
            s.sendMessage(ParamMap.stringify(pm))
          })
    }
  }

  def getUserByCondition(pm: ParamMap, s: IImSocket) {
    var app = pm.getAppParamMap()
    //    val age = app.getOrElse("age", "")
    val gender = app.getOrElse("gender", "")
    val g = gender match {
      //      case "u" => ""
      case "m" => "男"
      case "f" => "女"
      case _ => ""
    }
    val e = new ImUser()
    e.setGender(g)
    val dto = ImUserBiz.getUserByConditions(e)
    DtoHelper.doForDto(dto, () => {
      pm.setAppParamMap(immutable.Map("users" -> dto.getEntity))
      s.sendMessage(ParamMap.stringify(pm))
    }, null)
  }

  def getServerUnreadImMsgs(pm: ParamMap, s: IImSocket) {
    var app = pm.getAppParamMap()

    val leoQqMsg = new ImMsg
    leoQqMsg.setToUserNum(s.getNum)
    val dto = ImMsgBiz.queryUnreadImMsg(leoQqMsg)
    DtoHelper.doByDto(dto, () => {
      if (dto.entity.length > 0) {
        val responseMap = ParamMap()
        responseMap.setSysParamMap(Map(ParamMap.action -> ImActionMap.leoMainMsg, ParamMap.method -> "getServerUnreadImMsgs"))
        responseMap.setAppParamMap(Map("leoQqMsgs" -> dto.getEntity))
        s.sendMessage(ParamMap.stringify(responseMap))
        //            LeoSocketManager.doInLoginSockets(s.getNum, (toSocket: ILeoSocket) => {
        //              val ret = toSocket.sendMessage(ParamMap.stringify(responseMap))
        //            })

        // mark as read, begin
        //        dto.getEntity.foreach[List[String]](e=> yield  e.getMsgUuid)
        val l = for (i <- 0 to dto.getEntity.length - 1)
          yield dto.getEntity()(i).getMsgUuid
        ImMsgBiz.updateLeoImMsgReadByUuids(l.toList)
        // mark as read, end
      }
    }, null)

    //    dto match {
    //      case null => //TODO
    //      case _ =>
    //        dto.getIsSuccess match {
    //          case false => //TODO
    //          case true =>
    //            val responseMap = ParamMap()
    //            responseMap.setSysParamMap(Map(ParamMap.action -> leoMainMsg, ParamMap.method -> "getServerUnreadImMsgs"))
    //            responseMap.setAppParamMap(Map("leoQqMsgs" -> dto.getEntity))
    //            s.sendMessage(ParamMap.stringify(responseMap))
    //          //            LeoSocketManager.doInLoginSockets(s.getNum, (toSocket: ILeoSocket) => {
    //          //              val ret = toSocket.sendMessage(ParamMap.stringify(responseMap))
    //          //            })
    //
    //          // mark as read, begin
    //
    //          // mark as read, end
    //
    //        }
    //    }
  }

  @deprecated
  def getLatestLeoQqMsg(pm: ParamMap, s: IImSocket) {
    var appParamMap = pm.getAppParamMap()
    if (appParamMap.contains("fromUserNum")) {
      val fromUserNum = appParamMap("fromUserNum")
      fromUserNum match {
        case x: String =>
          val leoQqMsg = new ImMsg()
          leoQqMsg.setFromUserNum(x)
          leoQqMsg.setToUserNum(s.getNum)
          val leoDto = ImMsgBiz.getLatestLeoQqMsg(leoQqMsg)
          if (leoDto.isSuccess) {
            val responseMap = ParamMap()
            responseMap.setSysParamMap(pm.getSysParamMap)
            responseMap.setAppParamMap(immutable.Map("fromUserNum" -> x, "msgs" -> leoDto.getEntity))
            s.sendMessage(ParamMap.stringify(responseMap))

          } else {
            //TODO
          }
        case _ =>
      }
    }
  }
  def doLogin(pm: ParamMap, s: IImSocket) {
    LeoLogger.log("login action")
    var appParamMap = pm.getAppParamMap()
    if (appParamMap.contains("connectType")) {
      val connectType = appParamMap("connectType")
      LeoLogger.log("connectType is " + connectType)
      connectType match {
        case null =>
          LeoLogger.log("connectType is null")
          return
        case "login" =>
          val _num = appParamMap.getOrElse("num", "") //if no this key, return ""
          val _pwd = appParamMap.getOrElse("pwd", "") //if no this key, return ""
          LeoLogger.log("num is " + _num + ", pwd is " + _pwd)
          (_num, _pwd) match {
            case (x: String, y: String) if (!"".equals(x) && x != null && !"".equals(y) && y != null) =>
              val num = _num.toString
              val pwd = _pwd.toString
              val user = new ImUser()
              user.setNum(num)
              user.setPwd(pwd)
              // invoke biz and dao
              val ret = ImUserBiz.loginUser(user)
              LeoLogger.log("ret.getIsSuccess: " + ret.getIsSuccess)
              ret.getIsSuccess match {
                case true => //login OK
                  s.setIsLogin(true)
                  s.setNum(num)
                  // about local storage, begin
                  val localStorageSupported = appParamMap.getOrElse("storage", "false")
                  LeoLogger.logDebug("localStorageSupported is " + localStorageSupported + ", class:" + localStorageSupported.getClass)
                  if (true == localStorageSupported || "true".equals(localStorageSupported)) {
                    s.setLocalStorageSupported(true)
                    LeoLogger.logDebug("localStorageSupported is set to true")
                  } else {
                    s.setLocalStorageSupported(false)
                    LeoLogger.logDebug("localStorageSupported is set to false")
                  }
                  // about local storage, end
                  LeoSocketManager.switchToLogin(num, s)
                  // set some attributes, but not for SAE, start
                  s.setMaxIdleTime(500000)
                  s.setMaxBinaryMessageSize(800000000)
                  s.setMaxTextMessageSize(800000010)
                  // set some attributes, but not for SAE, end
                  val responseMap = ParamMap()
                  responseMap.setSysParamMap(pm.getSysParamMap)
                  responseMap.setAppParamMap(immutable.Map("num" -> num, "loginResult" -> "0", "loginMsg" -> "login successfully"))
                  s.sendMessage(ParamMap.stringify(responseMap))
                case false => //login failed
                  val responseMap = ParamMap()
                  responseMap.setSysParamMap(pm.getSysParamMap)
                  responseMap.setAppParamMap(immutable.Map("num" -> num, "loginResult" -> "2", "loginMsg" -> "用户名或密码不正确"))
                  s.sendMessage(ParamMap.stringify(responseMap))
              }
            case _ =>
              LeoLogger.log("invalid username or password.")
              val responseMap = ParamMap()
              responseMap.setSysParamMap(pm.getSysParamMap)
              responseMap.setAppParamMap(immutable.Map("num" -> _num, "loginResult" -> "1", "loginMsg" -> "请输入正确的用户名和密码."))
              s.sendMessage(ParamMap.stringify(responseMap))
              return
          }
        //case "login" end ---------------
        case _ => return
      }
      //connectType match end--------
    }
    //end of if
  } //end of method

  def sendImg(pm: ParamMap, s: IImSocket) { // send one img
    val app = pm.getAppParamMap
    val toUserNum = app.getOrElse("toUserNum", "") //if no this key, return ""
    val img = app.getOrElse("img", "") //if no this key, return ""
    LeoLogger.log(img.getClass())
    (toUserNum, img) match {
      case (x: String, _img: String) if (LeoUtil.isStringNotEmpty(Array(x, _img))) =>
        val bs = _img.getBytes("UTF-8")
        val fos = new FileOutputStream(new File("d:/a.jpg"))
        //        val bytes = new Array[Byte](fis.available())
        fos.write(bs);
        fos.close();
      case _ =>
        LeoLogger.log("invalid params")
    }
  }

  def sendMsgTemplate_backup(senderNum: String, receiverrNum: String, entity: LeoImEntity) = {
    LeoLogger.logDebug("-------sendMsgTemplate is called-----------")
    // -------response to the sender, begin
    val senderResponseMap = ParamMap()
    senderResponseMap.setSysParamMap(Map(ParamMap.action -> ImActionMap.leoMainMsg, ParamMap.method -> "receiveMyImMsg"))
    senderResponseMap.setAppParamMap(Map("leoQqMsgs" -> List(entity)))
    LeoSocketManager.doInLoginSockets(senderNum,
      // callback function begin
      (_socket: IImSocket) => {
        _socket.getLocalStorageSupported match {
          case true =>
            _socket.sendMessage(ParamMap.stringify(senderResponseMap))
          case false => // TODO: LocalStorage isn't supported on the client
        }
      } // callback function end
      ) //doInLoginSockets end
    // -------response to the sender, end

    // -------response to the receiver, begin
    var isSentToReceiver = false
    val receiverResponseMap = ParamMap()
    receiverResponseMap.setSysParamMap(Map(ParamMap.action -> ImActionMap.leoMainMsg, ParamMap.method -> "receiveImMsg"))
    receiverResponseMap.setAppParamMap(Map("leoQqMsgs" -> List(entity)))
    LeoSocketManager.doInLoginSockets(receiverrNum,
      // callback function begin
      (toSocket: IImSocket) => {
        toSocket.getLocalStorageSupported match {
          case true =>
            val ret = toSocket.sendMessage(ParamMap.stringify(receiverResponseMap))
            if (ret) {
              isSentToReceiver = true; //one socket send msg OK, then true
            }
          case false => //TODO
        }
      } // callback function end
      ) //doInLoginSockets end
    // -------response to the receiver, end
    isSentToReceiver
  }

  /**
   * return: Unit/Void
   * steps:
   * 1. response to receiver
   * 2. save msg to DB
   * 3. response to sender
   */
  def sendMsgTemplate[T](senderNum: String, receiverNum: String,
    saveDb: (Boolean) => LeoDTO[T],
    broadcastToSender: Boolean,
    saveDbSuccess: (IImSocket) => Boolean,
    receiverStorageSupported: (IImSocket) => Boolean,
    saveDbFail: (IImSocket) => Boolean): Unit = {

    LeoLogger.logDebug("-------sendMsgTemplate is called-----------")
    // -------response to the receiver, begin
    var isSentToReceiver = false
    LeoSocketManager.doInLoginSockets(receiverNum,
      // callback function begin
      (toSocket: IImSocket) => {
        toSocket.getLocalStorageSupported match {
          case true =>
            val ret = receiverStorageSupported(toSocket)
            if (ret) {
              isSentToReceiver = true; //one socket send msg OK, then true
            }
          case false => //TODO
        }
      } // callback function end
      ) //doInLoginSockets end
    // -------response to the receiver, end

    val dto = saveDb(isSentToReceiver) //save to DB

    // -------response to the sender, begin
    DtoHelper.doForDto(dto,
      //dto success callback, begin
      () => {
        if (broadcastToSender) {
          //doInLoginSockets start
          LeoSocketManager.doInLoginSockets(senderNum,
            // callback function begin
            (_socket: IImSocket) => {
              _socket.getLocalStorageSupported match {
                case true =>
                  saveDbSuccess(_socket)
                case false => // TODO: LocalStorage isn't supported on the client
              }
            } // callback function end
            ) //doInLoginSockets end
        } else {
          saveDbSuccess(null)
        }
      } //dto success callback, end
      ,
      //dto fail callback, begin
      () => {
        if (saveDbFail != null) {
          LeoSocketManager.doInLoginSockets(senderNum,
            // callback function begin
            (_socket: IImSocket) => {
              _socket.getLocalStorageSupported match {
                case true =>
                  saveDbFail(_socket)
                case false => // TODO: LocalStorage isn't supported on the client
              }
            } // callback function end
            ) //doInLoginSockets end
        }
      } //dto fail callback, begin
      )
    // -------response to the sender, end
  }

  def sendMsg(pm: ParamMap, s: IImSocket) { // send one msg
    //    LeoSocketManager.getLoginSockets
    val app = pm.getAppParamMap
    val toUserNum = app.getOrElse("toUserNum", "") //if no this key, return ""
    val content = app.getOrElse("content", "") //if no this key, return ""
    val _msgUuid = LeoUtil.genUUID()
    (toUserNum, content) match {
      case (_to: String, _content: String) if (LeoUtil.isStringNotEmpty(Array(_to, _content))) =>
        val leoQqMsg = new ImMsg() {
          fromUserNum = s.getNum
          toUserNum = _to
          content = _content
          msgUuid = _msgUuid
          creationTime = LeoUtil.getCurTime
        }

        // response to sender and receiver, begin
        sendMsgTemplate(s.getNum, _to,
          (isSentToReceiver: Boolean) => {
            // saved to DB
            leoQqMsg.setIsRead(if (isSentToReceiver) "1" else "0")
            ImMsgBiz.insertLeoQqMsg(leoQqMsg)
          },
          true,
          (_socket: IImSocket) => {
            // success response to sender
            val senderResponseMap = ParamMap()
            senderResponseMap.setSysParamMap(Map(ParamMap.action -> ImActionMap.leoMainMsg, ParamMap.method -> "receiveMyImMsg"))
            senderResponseMap.setAppParamMap(Map("leoQqMsgs" -> List(leoQqMsg)))
            _socket.sendMessage(ParamMap.stringify(senderResponseMap))
          },
          (_socket: IImSocket) => {
            // response to receiver
            val receiverResponseMap = ParamMap()
            receiverResponseMap.setSysParamMap(Map(ParamMap.action -> ImActionMap.leoMainMsg, ParamMap.method -> "receiveImMsg"))
            receiverResponseMap.setAppParamMap(Map("leoQqMsgs" -> List(leoQqMsg)))
            _socket.sendMessage(ParamMap.stringify(receiverResponseMap))
          }, null)
      // response to sender and receiver, end

      case _ =>
        LeoLogger.log("invalid parameters: " + (toUserNum, content)) //invalid  fromUserNum
    } //match end
  }

  def main(args: Array[String]) {
    //    val r = collection.Map[Int, Int]((2, 3), (5, 6), 4 -> 5)
    //    val r2 = collection.Map[String, Int](("2", 3), ("5", 6), "4" -> 5)
    //    println(r(8))
    //    val r = ActionMap.actionMap("leoLogin").get("loginsss")
    //    println(r)

    //    val i = 3
    //    i match {
    //      case x: Int if (x > 8) => println("haha1")
    //      case _ => println("else")
    //    }

    //    val bs = _img.getBytes("UTF-8")
    val fos = new FileOutputStream(new File("d:/a.jpg"))
    //        val bytes = new Array[Byte](fis.available())
    val fis = new FileInputStream("d:/2012824161039699.JPG");
    //    val inputArr = new Array[Byte](fis.available())
    val inputArr = new Array[Byte](fis.available())
    fis.read(inputArr)
    fos.write(inputArr);
    fos.close();

  }
}

object ImFriendAction {
  def agreeAddFriend(pm: ParamMap, socket: IImSocket) {
    val app = pm.getAppParamMap
    val fromUserNum = app.getOrElse("fr", "")

    LeoUtil.convertToString(fromUserNum) match {
      case "" =>
      case _from =>

        // sender add receiver as friend, begin
        val e = new ImRelation
        e.setFromUserNum(_from)
        e.setToUserNum(socket.getNum)
        e.setCreationTime(LeoUtil.getCurTime)
        val insertDto = ImRelationBiz.insert(e)
        DtoHelper.doForDto(insertDto, () => {
          insertDto.entity match {
            case 0 => //already friends
            case 1 =>
              // get sender's info and send to receiver, begin
              val getFromUserDto = ImUserBiz.getUserByNum(_from)
              DtoHelper.doForDto(getFromUserDto, () => {
                pm.setAppParamMap(immutable.Map("ret" -> "1", "users" -> List(getFromUserDto.getEntity)))
                socket.sendMessage(ParamMap.stringify(pm))
              }, null)
              // get receiver's info and send to receiver, end

              // get receiver's info and send to sender, begin
              if (LeoSocketManager.checkIsLoginByNum(_from)) {
                val getReceiverDto = ImUserBiz.getUserByNum(socket.getNum)
                DtoHelper.doForDto(getReceiverDto, () => {
                  LeoSocketManager.doInLoginSockets(_from, (_socket) => {
                    val _pm = ParamMap()
                    _pm.setSysParamMap(Map(ParamMap.action -> ImActionMap.imFriend, ParamMap.method -> "agreeAddFriendForSender"))
                    _pm.setAppParamMap(immutable.Map("users" -> List(getReceiverDto.getEntity)))
                    _socket.sendMessage(ParamMap.stringify(_pm))
                  })
                }, null)
              }
            // get receiver's info and send to sender, end
          } //match end
        }, null) //doForDto end
        // sender add receiver as friend, end

        // receiver add sender as friend, begin
        val e2 = new ImRelation
        e2.setFromUserNum(socket.getNum)
        e2.setToUserNum(_from)
        e2.setCreationTime(LeoUtil.getCurTime)
        val insertDto2 = ImRelationBiz.insert(e2)
        DtoHelper.doForDto(insertDto2, () => {
          insertDto2.entity match {
            case 0 => //already friends
            case 1 =>
              // get receiver's info and send to sender, begin
              if (LeoSocketManager.checkIsLoginByNum(_from)) {
                val getReceiverDto = ImUserBiz.getUserByNum(socket.getNum)
                DtoHelper.doForDto(getReceiverDto, () => {
                  LeoSocketManager.doInLoginSockets(_from, (_socket) => {
                    val _pm = ParamMap()
                    _pm.setSysParamMap(Map(ParamMap.action -> ImActionMap.imFriend, ParamMap.method -> "agreeAddFriendForSender"))
                    _pm.setAppParamMap(immutable.Map("users" -> List(getReceiverDto.getEntity)))
                    _socket.sendMessage(ParamMap.stringify(_pm))
                  })
                }, null)
              }
            // get receiver's info and send to sender, end
          } //match end
        }, null) //doForDto end
      // receiver add sender as friend, end
    }
  }
}