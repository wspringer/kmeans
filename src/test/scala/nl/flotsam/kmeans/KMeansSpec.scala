package nl.flotsam.kmeans

import org.specs2.mutable.Specification

import EuclidianGeometry.g
import KMeans._

class KMeansSpec extends Specification {

  "KMeans" should {
    val points = List(
      (1.0, 1.0),
      (1.5, 2.0),
      (3.0, 4.0),
      (5.0, 7.0),
      (3.5, 5.0),
      (4.5, 5.0),
      (3.5, 4.5)
    )
    val clustered = cluster(points, 2)
    println(clustered)
    clustered must not beNull
  }

}
