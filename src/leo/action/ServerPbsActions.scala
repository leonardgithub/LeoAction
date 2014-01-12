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

object PbsActionMap {

  //  val actionMap: immutable.Map[String, AnyRef] = immutable.Map(
  //    "leoSendMsg" -> MsgAction,
  //    "leoFindFriend" -> FriendAction,
  //    "leoLogin" -> LoginAction,
  //    "leoList" -> LeoQqListAction, "" -> LoginAction.f3, "" -> LoginAction.f)

  val actionMap: immutable.Map[String, immutable.Map[String, (ParamMap) => String]] = immutable.Map(
    "pbsImg" -> immutable.Map(
      "queryImgsByTag" -> PbsImgAction.queryImgsByTag,
      "xxxxx" -> null))

  def getHandler(action: String, method: String) = {
    if (actionMap.contains(action)) {
      val m = actionMap(action)
      if (m.contains(method)) {
        m(method)
      } else {
        null
      }
    } else {
      null
    }
  }
}

object PbsActionController {
  def doAction(data: String): String = {
    LeoLogger.log("PbsActionController.doAction start")
    val paramMap = ParamMap(data)
    var sysParamMap = paramMap.getSysParamMap()
    var appParamMap = paramMap.getAppParamMap()

    if (sysParamMap.contains("action") && sysParamMap.contains("method")) {
      val action = sysParamMap("action")
      val method = sysParamMap("method")
      val m = PbsActionMap.getHandler(action, method)
      if (m == null) {
        LeoLogger.log("no action or method")
        ""
      } else {
        m(paramMap)
      }
    } else { //invalid parameters
      LeoLogger.log("no action or method")
      ""
    }
  }
}

object PbsImgAction {
  def main(args: Array[String]) {
    println(List("", "").getClass())
    //    List.apply<Int>(1)
  }

  def queryImgsByTag(pm: ParamMap): String = {
    val app = pm.getAppParamMap
    val tag = app.getOrElse("tag", "")
    tag match {
      case t: String if (LeoUtil.isStringNotEmpty(t)) =>
        val dto = PbsImgBiz.queryByTag(t)
        DtoHelper.doForDto(dto, () => {
          dto.getEntity.foreach(e => {
            LeoLogger.log(e.getId)
          })
        }, null)
      case _ =>
    }
    ""
  }
  //return json
  def savePbsImg(pm: ParamMap, savedFileNames: Array[String]): String = {
    LeoLogger.log("pm: " + pm);
    var app = pm.getAppParamMap()

    val imgs = app.getOrElse("imgs", Nil)
    imgs match {
      case l: List[Map[String, Any]] if l != Nil =>
        //        if (l.length != savedFileNames.length) {
        //          return "" //TODO
        //        }
        for (i <- 0 to savedFileNames.length - 1) {
          val img = l(i)
          //        }
          //        l.foreach(img => {
          val e = new PbsImg()
          val tag1 = img.getOrElse("tag1", "")
          val tag2 = img.getOrElse("tag2", "")
          val tag3 = img.getOrElse("tag3", "")
          val tag4 = img.getOrElse("tag4", "")
          (tag1, tag2, tag3, tag4) match {
            case (t1: String, t2: String, t3: String, t4: String) =>
              e.setTag1(t1)
              e.setTag2(t2)
              e.setTag3(t3)
              e.setTag4(t4)
            case _ =>
          }
          e.setImgPath(savedFileNames(i))
          e.setUserNum("")
          e.setImgGroup(-1)
          e.setIsDeleted(false)
          e.setLikeCount(0)
          e.setCreationTime(LeoUtil.getCurTime)
          PbsImgBiz.insert(e)
        }
        ""
      case _ => ""
    }
  }
}