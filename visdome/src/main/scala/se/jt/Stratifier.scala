package se.jt

import scala.collection.mutable.ArrayBuffer

object Stratifier {
	case class StratGroup(name:String, indexes:Seq[Int])
	
	class Wrapper[D, T](
			f:D => T, 
			strat:Stratifier[T]
	) extends Stratifier[D] {
		def apply(data:Seq[D]):Stratification =
			strat(data.map(f))
	}
	
	class DoubleNStratifier(val n:Option[Int] = None) extends Stratifier[Double] {
		def apply(data:Seq[Double]) = {
			val min = data.min
			val max = data.max
			val n = this.n.getOrElse(math.max(10, data.length / 100))
			val w = (max - min) / n
			val borders = for (i <- 0 until (n+1)) yield min + w*i
			val names = for (i <- 0 until n) yield 
				"[%.1f,%.1f)".format(borders(i), borders(i+1))
			val indexes = new Array[Int](data.length)
			val sizes = new Array[Int](n).map(_ => 0)
			for (i <- 0 until data.length) {
				val ind = math.min(n-1, ((data(i)-min)/w).toInt)
				indexes(i) = ind
				sizes(ind) += 1
			}
			Stratification(names, sizes, indexes)
		}
	}
	
	class IntNStratifier(val n:Option[Int] = None) extends Stratifier[Int] {
		def apply(data:Seq[Int]) = {
			val min = data.min
			val max = data.max
			val n = this.n.getOrElse(math.max(10, data.length / 100))
			val w = (max - min) / n
			val borders = for (i <- 0 until (n+1)) yield min + w*i
			val names = for (i <- 0 until n) yield 
				"[%f,%f)".format(borders(i), borders(i+1))
			val indexes = new Array[Int](data.length)
			val sizes = new Array[Int](n).map(_ => 0)
			for (i <- 0 until data.length) {
				val ind = ((data(i)-min)/w).toInt
				indexes(i) = ind
				sizes(ind) += 1
			}
			Stratification(names, sizes, indexes)
		}
	}
	
	class StringStratifier extends Stratifier[String] {
		def apply(data:Seq[String]) = {
			val names = data.distinct
			val n = names.length
			val indexes = new Array[Int](data.length)
			val sizes = new Array[Int](n).map(_ => 0)
			for (i <- 0 until data.length) {
				val ind = names.indexOf(data(i))
				indexes(i) = ind
				sizes(ind) += 1
			}
			Stratification(names, sizes, indexes)
		} 
			
	}
	
	implicit val doubleStratifier:Stratifier[Double] 	= new DoubleNStratifier
	implicit val intStratifier:Stratifier[Int] 			= new IntNStratifier
	implicit val stringStratifier:Stratifier[String] 	= new StringStratifier
	
}

trait Stratifier[T] extends Function1[Seq[T], Stratification] {
	def apply(data:Seq[T]):Stratification
}

class StratifiedScale[T](
		val groups:Stratification
) extends Categorical[T] with NoSetup[T] {
	type Self = StratifiedScale[T]
	
	def apply(t:T, i:Int) =
		relPos(groups.values(i))
	
	def relPos(groupIndex:Int) =
		(1+2*groupIndex.toDouble) / (2*groups.length)
		
	def ticks = (0 until groups.length).map(i => (groups.names(i), relPos(i)))
}

case class Stratification(
		val names:Seq[String], 
		val sizes:Seq[Int], 
		val values:Seq[Int]
) extends LevelCount {
	val depth = 1
	val levels = (0 until length).map(i => Array(i).toSeq).toSeq
	val counts = sizes 
	def length = names.length
	def relGroupValues = (0 until length).map(_ / (length - 1.0))
	
	def overlay(s:Stratification):Stratification = {
		val zips = values.zip(s.values)
		val bins = zips.distinct
		val sizes = new Array[Int](bins.length).map(_ => 0)
		val indexes = new Array[Int](length)
		for (i <- 0 until length) {
			val ind = bins.indexOf(zips(i))
			indexes(i) = ind
			sizes(ind) += 1
		}
		Stratification(bins.map(_.toString), sizes, indexes)
	}
}

object JoinCount {
	def apply(s1:Stratification, s2:Stratification):JoinCount = {
		val keys = Array(s1.values, s2.values).toSeq.transpose
		val levels = keys.distinct
		val counts = new Array[Int](levels.length)
		for (i <- 0 until keys.length) {
			counts(levels.indexOf(keys(i))) += 1
		}
		JoinCount(levels, counts)
		
	}
}

trait LevelCount {
	def depth:Int
	def levels:Seq[Seq[Int]]
	def counts:Seq[Int]
	def keyCount = levels.zip(counts)
}

case class JoinCount(val levels:Seq[Seq[Int]], val counts:Seq[Int]) extends LevelCount {
	def depth = levels.head.length
}