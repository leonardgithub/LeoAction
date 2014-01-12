package leo.web.uitl

import scala.beans.BeanProperty
import scala.util.parsing.json._
import leo.util._
import scala.collection._
import leo.entity._
import java.util.Date

class ParamMap {
  @BeanProperty var sysParamMap: immutable.Map[String, String] = null
  @BeanProperty var appParamMap: immutable.Map[String, Any] = null
}
object ParamMap {
  // constants, begin
  def sysParams() = "s"
  def appParams() = "a"
  val action = "action"
  val method = "method"
  val doubleQuotationInJson = "\""
  // constants, end

  //stringify json to string, begin
  def stringifyAny(o: Any): String = {
    o match {
      //      case null =>
      //        LeoLogger.log("stingify null")
      //        "null"
      case x: Char =>
        stringify(x)
      case x: String =>
        stringify(x)
      case x: collection.Map[String, Any] =>
        stringify(x)
      case x: List[Any] =>
        stringify(x)
      case x: Array[Any] =>
        stringify(x)
      case x: Date =>
        stringify(x)
      case x: ParamMap =>
        stringify(x)
      //      case x: leo.entity.LeoEntity =>
      //        stringify(x)
      case x: ImFriendGroup =>
        stringify(x)
      case x: ImUser =>
        stringify(x)
      case x: ImRelation =>
        stringify(x)
      case x: ImMsg =>
        stringify(x)
      case x: ImAddFriendMsg =>
        stringify(x)
      case _ =>
        LeoLogger.log("stingify _")
        "{}"
    }
  }
  def stringify(o: Char): String = {
    stringify(o.toString);
  }
  def stringify(o: String): String = {
    if (o == null || "".equals(o)) {
      val s = "\"\""
      //      LeoLogger.log("stingify string: " + s)
      s
    } else {
      // can not change the replacement order
      val s = "\"" + o.replaceAll("\\\\", "\\\\\\\\").replaceAll("\"", "\\\\\"").replaceAll("\n", "\\\\n") + "\""
      //      LeoLogger.log("stingify string " + o + " to " + s)
      s
    }
  }

  def stringify(d: Date): String = {
    if (d == null) {
      val s = "\"\""
      //      LeoLogger.log("stingify Date: " + s)
      s
    } else {
      val s = "\"" + LeoUtil.getTimeStr(d) + "\""
      //      LeoLogger.log("stingify Date(" + d + ") to " + s)
      s
    }
  }

  def stringify(e: ImFriendGroup): String = {
    //    LeoLogger.log("stingify LeoQqList")
    if (e == null) {
      return "{}"
    }
    val retJsonStr = "\"id\":\"" + e.getId + "\",\"listName\":\"" + e.getListName + "\",\"toUsers\":" + stringify(e.getToUsers)

    "{" + retJsonStr + "}"
  }

  def stringify(e: ImRelation): String = {
    //    LeoLogger.log("stingify LeoQqRelation")
    if (e == null) {
      return "{}"
    }
    val retJsonStr = "\"id\":\"" + e.getId + "\",\"toUserNum\":\"" + e.getToUserNum + "\",\"toUserNumAlias\":" + stringify(e.getToUserNumAlias) + ",\"toUser\":" + stringify(e.getToUser)

    "{" + retJsonStr + "}"
  }

  def stringify(e: ImMsg): String = {
    if (e == null) {
      // LeoLogger.log("stingify LeoQqMsg: {}")
      "{}"
    } else {
      var retJsonStr = "\"id\":\"" + e.getId + "\",\"fromUserNum\":\"" + e.getFromUserNum + "\",\"content\":" + stringify(e.getContent) + ",\"msgUuid\":" + stringify(e.getMsgUuid) + ",\"toUserNum\":" + stringify(e.getToUserNum) + ",\"creationTime\":" + stringify(e.getCreationTime)
      retJsonStr = "{" + retJsonStr + "}"
      // LeoLogger.log("stingify LeoQqMsg: " + retJsonStr)
      retJsonStr
    }
  }

  def stringify(e: ImAddFriendMsg): String = {
    if (e == null) {
      LeoLogger.log("stingify LeoImAddFriend: {}")
      "{}"
    } else {
      var retJsonStr = "\"id\":\"" + e.getId + "\",\"fr\":\"" + e.getFromUserNum + "\",\"ct\":" + stringify(e.getContent) + ",\"to\":" + stringify(e.getToUserNum) + ",\"time\":" + stringify(e.getCreationTime)
      retJsonStr = "{" + retJsonStr + "}"
      LeoLogger.log("stingify LeoImAddFriend: " + retJsonStr)
      retJsonStr
    }
  }

  def stringify(e: ImUser): String = {
    //    LeoLogger.log("stingify LeoQqUser")
    if (e == null) {
      return "{}"
    }
    val retJsonStr = "\"id\":" + e.getId + ",\"num\":" + stringify(e.getNum) + ",\"nickname\":" + stringifyAny(e.getNickname()) + ",\"gender\":" + stringify(e.getGender)

    "{" + retJsonStr + "}"
  }

  def stringify(pm: ParamMap): String = {
    //    LeoLogger.log("stingify ParamMap")
    stringify(Map(sysParams -> pm.getSysParamMap, appParams -> pm.getAppParamMap))
  }

  def stringify(l: List[Any]): String = {
    //    LeoLogger.log("stingify List")
    if (l == null || l.size == 0) {
      val ret = "[]"
      //      LeoLogger.log("stingify List: " + ret)
      return ret
    }

    var retJsonStr = ""
    for (t <- l) {
      if ("" != retJsonStr) { //if not first element
        retJsonStr += ","
      }
      retJsonStr += "" + stringifyAny(t)
    }

    retJsonStr = "[" + retJsonStr + "]"
    //    LeoLogger.log("stingify List: " + retJsonStr)
    retJsonStr
  }

  def stringify(l: Array[Any]): String = {
    if (l == null || l.size == 0) {
      val ret = "[]"
      //      LeoLogger.log("stingify Array: " + ret)
      return ret
    }

    var retJsonStr = ""
    for (t <- l) {
      if ("" != retJsonStr) { //if not first element
        retJsonStr += ","
      }
      retJsonStr += "" + stringifyAny(t)
    }

    retJsonStr = "[" + retJsonStr + "]"
    //    LeoLogger.log("stingify Array: " + retJsonStr)
    retJsonStr
  }

  def stringify(m: collection.Map[String, Any]): String = {
    LeoLogger.log("stingify Map")
    if (m == null || m.size == 0) {
      return "{}"
    }

    var retJsonStr = ""
    for (t <- m) {
      if ("" != retJsonStr) {
        retJsonStr += ","
      }
      retJsonStr += "\"" + t._1 + "\":" + stringifyAny(t._2)
    }

    "{" + retJsonStr + "}"
  }
  //stringify json to string, begin

  //create a ParamMap, begin
  def apply(): ParamMap = {
    val paramMap = new ParamMap()
    paramMap
  }
  def apply(a: String, m: String): ParamMap = {
    val paramMap = new ParamMap()
    paramMap.setSysParamMap(immutable.Map(action -> a, method -> m))
    paramMap
  }
  def apply(s: String): ParamMap = {
    val result = JSON.parseFull(s)
    val r = result match {
      case Some(e) =>
        e.asInstanceOf[Map[String, AnyRef]]
      case None => Map[String, AnyRef]()
    }
    LeoLogger.log("ParamMap is " + r)
    val paramMap = new ParamMap()
    if (r.contains(sysParams)) {
      r(sysParams) match {
        case m: Map[String, String] => paramMap.setSysParamMap(r(sysParams).asInstanceOf[immutable.Map[String, String]])
        case _ => paramMap.setSysParamMap(immutable.Map())
      }
    } else {
      paramMap.setSysParamMap(immutable.Map())
    }

    if (r.contains(appParams)) {
      r(appParams) match {
        case m: Map[String, Map[String, Any]] => paramMap.setAppParamMap(r(appParams).asInstanceOf[immutable.Map[String, AnyRef]])
        case _ => paramMap.setAppParamMap(immutable.Map())
      }
    } else {
      paramMap.setAppParamMap(immutable.Map())
    }
    paramMap
  }
  //create a ParamMap, end

  def main(args: Array[String]) {
    this.stringify('o')
  }
}