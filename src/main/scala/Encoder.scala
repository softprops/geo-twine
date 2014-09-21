package geotwine

import geotwine.Geohash.{ Alphabet, Box }

object Encoder {
  def encode(box: Box): String = {
    val mins = stream(box.minLat, box.minLon)
    val maxes = stream(box.maxLat, box.maxLon)
    (mins zip maxes)
      .takeWhile { case (x, y) => x ==y }
      .take(32)
      .map { case (x, _) => x }
      .mkString
  }

  def encode(lat: Double, lon: Double, level: Int = 5) =
    stream(lat, lon).take(level).mkString

  def stream(lat: Double, lon: Double): Stream[Char] = {
    def chars(bits: Stream[Boolean]): Stream[Char] = {
      val char = (0 /: bits.take(5)) {
        (a, bit) =>
          if (bit) (2 * a) + 1
          else 2 * a
      }
      Stream.cons(Alphabet(char), chars(bits.drop(5)))
    }
    def bools(x: Double, min: Double, max: Double): Stream[Boolean] = {
      val mid = (min + max) / 2
      if (x >= min) Stream.cons(true, bools(x, mid, max))
      else Stream.cons(false, bools(x, min, max))
    }
    def twist[A](x: Stream[A], y: Stream[A]): Stream[A] =
      Stream.cons(x.head, twist(y, x.tail))
    chars(twist(bools(lat, -180, 180), bools(lon, -90, 90)))
  }
}
