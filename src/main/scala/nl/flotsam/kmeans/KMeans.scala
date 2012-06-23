package nl.flotsam.kmeans

import annotation.tailrec

object KMeans {

  def cluster[T](xs: Seq[T], k: Int)(implicit g: Geometry[T]): Seq[Seq[T]] = {
    @tailrec
    def step(xs: Seq[T], centroids: Seq[T]): Seq[Seq[T]] = {
      val labeled =
        for (x <- xs) yield {
          val distances = for ((centroid) <- centroids) yield (centroid, g.distance(x, centroid))
          val nearestCentroid = distances.minBy(_._2)._1
          (nearestCentroid, x)
        }
      val grouped = for (centroid <- centroids) yield labeled.collect({
        case (`centroid`, x) => x
      })
      val replacements = grouped.map(g.centroid)
      if (replacements.forall(replacement => centroids.exists(centroid => centroid == replacement))) {
        grouped
      } else {
        step(xs, replacements)
      }
    }
    step(xs, xs.take(k))
  }

}