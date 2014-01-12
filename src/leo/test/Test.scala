package leo.test

import scala.util.parsing.json.JSON
import java.util.Date
import leo.biz._
import leo.entity._

class MyRunnable extends Runnable {

  def run {
    Test.add
  }
}

object Test {
  def main(args: Array[String]) {
//    ImRelationBiz.main(null);
    ImFriendGroupBiz.main(null)
  }

  var count = 0

  def add() {
    synchronized("sss") {
      count = count + 1
      if (count == 1000) {
        println(count)
      }
      1
    }
  }

  def stringify(m: collection.Map[String, Any]): String = {
    if (m == null || m.size == 0) {
      return "{}"
    }
    var retJsonStr = ""
    for (t <- m) {
      if ("" != retJsonStr) {
        retJsonStr += ","
      }
      retJsonStr += "\"" + t._1 + "\":" + t._2
    }
    "{" + retJsonStr + "}"
  }

}