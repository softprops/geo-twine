package geotwine

object Geohash {
  private[geotwine] val Alphabet = "0123456789bcdefghjkmnpqrstuvwxyz"
  case class Box(
    minLon: Double, minLat: Double, maxLat: Double, maxLon: Double) {
    def center: (Double, Double) =
      ((minLat + maxLat) / 2.0, (minLon + maxLon) / 2.0)
  }
}
