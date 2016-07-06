package bimbo.data.dao.townstate

import scala.io.Source
import java.io.File
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger

case class TownStateDAO(townStateFile: String) {

  /**
   * key - depotId, value - townstate
   */
  def getTownStateMap(): immutable.Map[Int, TownState] = {

    val townI = new AtomicInteger()
    val townIdMap: mutable.Map[String, Int] = mutable.Map()

    val stateI = new AtomicInteger()
    val stateIdMap: mutable.Map[String, Int] = mutable.Map()

    val townStateMap: immutable.Map[Int, TownState] = Source.fromFile(new File(townStateFile)).getLines().drop(1).map { l =>

      val lArray = l.split(",")

      val depotId = lArray(0).toInt
      val townId = townIdMap.getOrElseUpdate(lArray(1), townI.getAndIncrement)
      val stateId = stateIdMap.getOrElseUpdate(lArray(2), stateI.getAndIncrement)
      depotId -> TownState(townId, stateId)
    }.toMap

    townStateMap
  }
}