package se.jt.iter2

import java.awt.Graphics2D
import Geom.Rect

trait Scale[T] {

	var visible = true
	
	def setup(data:Seq[T]):Unit 
	
	def apply(t:T):Double
	
	def ticks:Seq[(String, Double)]
}

trait GetScale[T] {
	def apply():Scale[T]
}


object Scale {
	
	class FixScale[D](val min:Double, val max:Double) extends Scale[D] {
		
		def setup(data:Seq[D]) = {}
		
		def apply(d:D):Double = 0.5
			
		def ticks = {
			val n = 5
			val d = (max-min) / n
			for (i <- 0 until n) yield { 
				val x = min + i*d
				("%.2f".format(x), (x - min)/(max-min))
			}
		}
	}
	
	class ContScale extends Scale[Double] {
		
		var min = 0.0
		var max = 0.0
		
		def setup(data:Seq[Double]) = {
			min = data.min
			max = data.max
		}
		
		def apply(d:Double):Double =
			(d - min) / (max - min)
			
		def ticks = {
			val n = 5
			val d = (max-min) / n
			for (i <- 0 until n) yield { 
				val x = min + i*d
				("%.2f".format(x), this(x))
			}
		}
	}
	
	class DiscreteScale extends Scale[String] {
		
		var values:Seq[String] = _
		
		def setup(data:Seq[String]) = 
			values = data.distinct
		
		def apply(s:String):Double =
			(1+2*values.indexOf(s).toDouble) / (2*values.length)
		
		def ticks = values.map(v => (v, this(v)))
	}
	
	class ScaleWrapper[D, T](f:D => T, scale:Scale[T]) extends Scale[D] {
		
		override def setup(data:Seq[D]) = scale.setup(data.map(f))
		def apply(d:D):Double = scale(f(d))
		def ticks = scale.ticks
	}
	
	implicit val doubleScale = new GetScale[Double] { def apply = new ContScale }
	implicit val stringScale = new GetScale[String] { def apply = new DiscreteScale}

}