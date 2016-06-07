package net.jvw

import scala.xml.{NodeSeq, XML}
import java.time.{Duration, Instant}

object Convert {

  val RADIUS = 6365 // Netherlands, https://rechneronline.de/earth-radius/
  val WINDOW = 2 // will even out eventually

  def processFile(path: String) = {
    val xml = XML.loadFile(path)
    val trackPoints = xml \\ "trkpt"
    // skip first x seconds because gps isn't accurate enough when just started
    val (left, right) = trackPoints.splitAt(40)
    output(right)
  }

  def distance(origin: (Double, Double), destination: (Double, Double)) = {
    // source : https://github.com/sdemers/GPX_median_speed/blob/master/gpx_median_speed.py
    // and for readability: http://www.movable-type.co.uk/scripts/latlong.html
    val (lat1, lon1) = origin
    val (lat2, lon2) = destination
    val phiLat1 = lat1.toRadians
    val phiLat2 = lat2.toRadians

    val dLat = math.toRadians(lat2 - lat1)
    val dLon = math.toRadians(lon2 - lon1)

    val a = math.sin(dLat / 2) * math.sin(dLat / 2) +
            math.cos(phiLat1) * math.cos(phiLat2) *
            math.sin(dLon / 2) * math.sin(dLon / 2)
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    val d = RADIUS * c
    d
  }

  def output(elements: NodeSeq): List[(Int, Double)] = {
    elements.sliding(WINDOW).map(nodeSeq => {
      val list = nodeSeq.toList

      val first = list.head
      val last = list.last

      val firstDateTime = (first \ "time") text
      val lastDateTime = (last \ "time") text

      val firstI: Instant = Instant.parse(firstDateTime)
      val lassI = Instant.parse(lastDateTime)
      val diffInSeconds = Duration.between(lassI, firstI).getSeconds

      val origin = (((first \ "@lat") text).toDouble, ((first \ "@lon") text).toDouble)
      val destination = (((last \ "@lat") text).toDouble, ((last \ "@lon") text).toDouble)

      val speedKph = Math.abs((distance(origin, destination) / diffInSeconds) * 3600)
      //TODO optional?
      val hr = ((last \\ "hr") text)
      val heartRate = if (hr == "") 0 else hr.toInt
      (heartRate, speedKph)

    }).toList

  }


}