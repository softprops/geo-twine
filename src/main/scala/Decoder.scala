package geotwine

import geotwine.Geohash.{ Alphabet, Box }

object Decoder {
  def decode(str: String): (Double, Double) =
    decodeBox(str).center

  def decodeBox(str: String): Box = {
    def range(bits: Stream[Boolean], min: Double, max: Double): (Double, Double) = {
      lazy val mid = (min + max) / 2
      if (bits.isEmpty) (min, max)
      else if (bits.head) range(bits.tail, mid, max)
      else range(bits.tail, min, mid)
    }
    def unwind[A](twisted: Stream[A]): (Stream[A], Stream[A]) =
      if (twisted.isEmpty) (Stream.empty, Stream.empty)
      else {
        val (xs, ys) = unwind(twisted.tail)
        (Stream.cons(twisted.head, ys), xs)
      }
    val bits = str.flatMap { (x: Char) => 
      val bitString = Alphabet.indexOf(x).toBinaryString
      ("00000".substring(0, 5 - bitString.length) + bitString).map('1' == _)
    }
    val (lons, lats) = unwind(bits.toStream)
    val (minLon, maxLon) = range(lons, -180, 180)
    val (minLat, maxLat) = range(lats, -90, 90)
    Box(minLon, maxLon, minLat, maxLat)
  }
}
