package nl.flotsam.kmeans

import annotation.tailrec

object KMeans {

  def cluster[T,U](xs: Seq[T], k: Int)(implicit fn: T => U, g: Geometry[U]): Seq[Seq[T]] = {
    case class Pair(original: T) {
      val projected = fn(original)
    }
    @tailrec
    def step(xs: Seq[Pair], centroids: Seq[U]): Seq[Seq[Pair]] = {
      val labeled =
        for (x <- xs) yield {
          val distances = for ((centroid) <- centroids) yield (centroid, g.distance(x.projected, centroid))
          val nearestCentroid = distances.minBy(_._2)._1
          (nearestCentroid, x)
        }
      val grouped = for (centroid <- centroids) yield labeled.collect({
        case (`centroid`, x) => x
      })
      val replacements = grouped.map(group => g.centroid(group.map(_.projected)))
      if (replacements.forall(replacement => centroids.exists(centroid => centroid == replacement))) {
        grouped
      } else {
        step(xs, replacements)
      }
    }
    val projectedXs = xs.map(Pair)
    val groupedMappings = step(projectedXs, projectedXs.take(k).map(_.projected))
    groupedMappings.map(_.map(_.original))
  }

  def cluster[T,U](fn: T => U)(xs: Seq[T], k: Int)(implicit g: Geometry[U]): Seq[Seq[T]] =
    cluster(xs, k)(fn, g)

}