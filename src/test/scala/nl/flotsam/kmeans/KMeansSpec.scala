package nl.flotsam.kmeans

import org.specs2.mutable.Specification

import EuclidianGeometry.g
import KMeans._

class KMeansSpec extends Specification {

  "KMeans" should {

    "allow you to cluster a list of points in a simple way" in {
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

    "allow you to cluster a list of things that can be mapped to points in a simple way" in {
      case class Coordinate(x: Double, y: Double)
      val points = List(
        Coordinate(1.0, 1.0),
        Coordinate(1.5, 2.0),
        Coordinate(3.0, 4.0),
        Coordinate(5.0, 7.0),
        Coordinate(3.5, 5.0),
        Coordinate(4.5, 5.0),
        Coordinate(3.5, 4.5)
      )
      def coordinateToPoint(coordinate: Coordinate) = (coordinate.x, coordinate.y)
      val clustered = cluster(coordinateToPoint)(points, 2)
      println(clustered)
      clustered must not beNull
    }
  }



}
