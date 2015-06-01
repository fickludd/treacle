package se.jt.iter1

import java.awt.Color
import scala.util.Random

object Util {
	def split[T, G](x:Iterable[T], g:Iterable[G]):Map[G, Iterable[T]] = {
		val keys = g.toSet
		(for (key <- keys) yield (
			key -> x.zip(g).filter(_._2 == key).map(_._1)
		)).toMap
	}
	
	def split[T, G](x:Seq[T], g:Seq[G]):Map[G, Seq[T]] = {
		val keys = g.toSet
		(for (key <- keys) yield (
			key -> x.zip(g).filter(_._2 == key).map(_._1)
		)).toMap
	}
	
	
	def randColor(k:Double) = 
		new Color(
				(math.min(1.0, 0.4 + Random.nextDouble*k)).toFloat, 
				(math.min(1.0, 0.4 + Random.nextDouble*k)).toFloat, 
				(math.min(1.0, 0.4 + Random.nextDouble*k)).toFloat
			)
}