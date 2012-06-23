package nl.flotsam.kmeans

trait Geometry[A] {
  def distance(x: A, y: A): Double

  def centroid(ps: Seq[A]): A
}

