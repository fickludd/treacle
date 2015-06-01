package se.jt

import java.awt.Graphics2D
import Geom.Rect

trait Scale[T] {
	
	type Self <: Scale[T]
	val self = this.asInstanceOf[Self]

	def setup(data:Seq[T]):Self
	
	def apply(t:T, i:Int):Double
	
	def apply(data:Seq[T]):Seq[Double] = 
		for (i <- 0 until data.length) yield apply(data(i), i)
	
	def border(cutoff:Double):(T,Int) => Boolean =
		cutoff <= this(_, _)
	
	def ticks:Seq[(String, Double)]
	
	def categorical:Boolean
}

trait NoSetup[T] extends Scale[T] {
	def setup(data:Seq[T]) = self
}

trait Continuous[T] extends Scale[T] {
	val categorical = false
}

trait Categorical[T] extends Scale[T] {
	val categorical = true
}

trait GetScale[T] {
	def apply():Scale[T]
	def apply(data:Seq[T]):Scale[T]
}


object Scale {
	
	def linTicks(min:Double, max:Double):Seq[Double] = {
		val w = max - min
		val ord = math.floor(math.log10(w))
		val itick = math.pow(10, ord)
		val tick =
			if (w / itick < 3) itick / 2
			else if (w / itick > 6) itick * 2
			else itick
		(math.ceil(min / tick).toInt to math.floor(max / tick).toInt).map(_ * tick) 
	}
	
	val niceNumberDecimalFormat = new java.text.DecimalFormat("#.####")
	def niceNumber(min:Double, max:Double)(d:Double):String = {
		if ((math.abs(max) < 0.001 && math.abs(min) < 0.001) || 
			math.abs(max) > 100000 || math.abs(min) > 100000)
			"%.1e".format(d)
		else niceNumberDecimalFormat.format(d)
	}
	
	
	class FixScale(
			val min:Double, 
			val max:Double
	) extends Continuous[Double] with NoSetup[Double] {
		type Self = FixScale
		
		val w = max - min
		val iMin = min - w*0.1
		val iMax = max + w*0.1
		
		def apply(d:Double, i:Int):Double = 
			(d - min) / (max - min)
	
		def ticks = {
			val nn = niceNumber(iMin, iMax) _
			for (t <- linTicks(iMin, iMax)) yield { 
				(nn(t), (t - iMin)/(iMax-iMin))
			}
		}
	}
	
	class DoubleScale extends Continuous[Double] {
		type Self = DoubleScale
		
		var min = 0.0
		var max = 0.0
		
		override def setup(data:Seq[Double]) = {
			val dmin = data.min
			val dmax = data.max
			val w = dmax - dmin
			min = dmin - w * 0.1
			max = dmax + w * 0.1
			this
		}
		
		def apply(d:Double, i:Int):Double =
			(d - min) / (max - min)
			
		def ticks = {
			val nn = niceNumber(min, max) _
			for (t <- linTicks(min, max)) yield { 
				(nn(t), this(t, 0))
			}
		}
	}
	
	class IntScale extends Scale[Int] {
		type Self = IntScale
		
		var min = 0
		var max = 1
		var categorical = true
		var values:Seq[Int] = _
		
		def setup(data:Seq[Int]) = {
			values = data.distinct.sorted
			if (values.length > 0) {
				min = data.min
				max = data.max
				categorical = false
			}
			this
		}
		
		def apply(d:Int, i:Int):Double =
			if (categorical)
				(1+2*values.indexOf(d).toDouble) / (2*values.length)
			else
				((d.toDouble - min + 1) / (max - min + 2))
			
		def ticks = {
			if (categorical)
				values.map(i => (i.toString, this(i, 0)))
			else {
				val w = max-min
				if (w < 10)
					for (x <- min to max) yield  
						("%d".format(x), this(x, 0))
				else {
					val n = 5
					for (i <- 0 until n) yield { 
						val x = (w * i) / n
						("%d".format(x), this(x, 0))
					}
				}
			}
		}
	}
	
	class Log10Scale extends Continuous[Double] {
		type Self = Log10Scale
		
		var logMin = 0.0
		var logMax = 0.0
		
		def setup(data:Seq[Double]) = {
			val dlogMin = math.log10(data.min)
			val dlogMax = math.log10(data.max)
			val w = dlogMax - dlogMin
			logMin = dlogMin - w * 0.1
			logMax = dlogMax + w * 0.1
			this
		}
		
		def apply(d:Double, i:Int):Double =
			(math.log10(d) - logMin) / (logMax - logMin)
			
		def ticks = {
			val n = 5
			val d = (logMax-logMin) / n
			for (i <- 0 until n) yield { 
				val x = logMin + i*d
				("%.1e".format(math.pow(10,x)), (x-logMin) / (logMax - logMin))
			}
		}
	}
	
	class DiscreteScale extends Categorical[String] {
		type Self = DiscreteScale
		
		var values:Seq[String] = _
		
		override def setup(data:Seq[String]) = { 
			values = data.distinct
			this
		}
		
		def apply(s:String, i:Int):Double =
			(1+2*values.indexOf(s).toDouble) / (2*values.length)
		
		def ticks = values.map(v => (v, this(v, 0)))
	}
	
	class Wrapper[D, T](f:D => T, scale:Scale[T]) extends Scale[D] {
		type Self = Wrapper[D, T]
		
		override def setup(data:Seq[D]) = {
			scale.setup(data.map(f))
			this
		}
		def apply(d:D, i:Int):Double = scale(f(d), i)
		def ticks = scale.ticks
		def categorical = scale.categorical
	}
	
	class GetWrapper[D, T](f:D => T, getScale:GetScale[T]) extends GetScale[D] {
		def apply() = new Wrapper(f, getScale())
		def apply(data:Seq[D]) = new Wrapper(f, getScale()).setup(data)
	}
	
	implicit val doubleScale = 
		new GetScale[Double] { 
			def apply = new DoubleScale
			def apply(data:Seq[Double]) = this().setup(data)
		}
	
	implicit val intScale = 
		new GetScale[Int] { 
			def apply = new IntScale 
			def apply(data:Seq[Int]) = this().setup(data)
		}
	
	implicit val stringScale = 
		new GetScale[String] { 
			def apply = new DiscreteScale
			def apply(data:Seq[String]) = this().setup(data)
		}
	
	val log10Scale = 
		new GetScale[Double] {
			def apply = new Log10Scale
			def apply(data:Seq[Double]) = this().setup(data)
		}
	
	val intLog10Scale = 
		new GetScale[Int] { 
			def apply = new Wrapper[Int, Double](_.toDouble, new Log10Scale) 
			def apply(data:Seq[Int]) = this().setup(data)	
		}

}