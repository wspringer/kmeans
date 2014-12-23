package nl.flotsam.kmeans

import annotation.tailrec
import util.Random

object KMeans {

  def pickRandom[T](xs: Seq[T], k: Int) = Random.shuffle(xs).take(k).distinct

  def cluster[T,U](xs: Seq[T], k: Int)
                  (implicit projection: T => U, space: VectorSpace[U]): Seq[Seq[T]] = {
    case class Pair(original: T) {
      val projected = projection(original)
    }
    @tailrec
    def step(xs: Seq[Pair], centroids: Seq[U]): Seq[Seq[Pair]] = {
      val labeled =
        for (x <- xs) yield {
          val distances = for ((centroid) <- centroids) yield (centroid, space.distance(x.projected, centroid))
          val nearestCentroid = distances.minBy(_._2)._1
          (nearestCentroid, x)
        }
      val grouped = for (centroid <- centroids) yield labeled.collect({
        case (`centroid`, x) => x
      })
      val replacements = grouped.map(group => space.centroid(group.map(_.projected)))
      val stable =
        replacements.forall {
          replacement =>
            centroids.exists(centroid => centroid == replacement)
        }
      if (stable) {
        grouped
      } else {
        step(xs, replacements)
      }
    }
    val associated = xs.map(Pair)
    val initial = pickRandom(associated.map(_.projected), k)
    step(associated, initial).map(_.map(_.original))
  }

  def cluster[T,U](fn: T => U)(xs: Seq[T], k: Int)
                  (implicit g: VectorSpace[U]): Seq[Seq[T]] = cluster(xs, k)(fn, g)

}