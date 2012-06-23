package nl.flotsam.kmeans

trait VectorSpace[A] {
  def distance(x: A, y: A): Double

  def centroid(ps: Seq[A]): A
}

