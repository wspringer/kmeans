package nl.flotsam.kmeans

import math._

object EuclidianGeometry {

  type Point = (Double, Double)

  implicit def g = new Geometry[Point] {
    def distance(x: Point, y: Point): Double =
      sqrt(pow(x._1 - y._1, 2) + pow(x._2 - y._2, 2))

    def centroid(ps: Seq[Point]): Point = {
      def pointPlus(x: Point, y: Point) = (x._1 + y._1, x._2 + y._2)
      ps.reduceLeft(pointPlus) match {
        case (a, b) => (a / ps.size, b / ps.size)
      }
    }
  }

}
