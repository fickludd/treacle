package se.jt.iter1

object ContScale {
	class LinScale(var min:Double, var max:Double) extends Scale {
		
		def apply(a:Any):Double = (a.asInstanceOf[Double] - min) / (max - min)
		def setDomain(a:Any*):Scale = {
			if (a.length != 2)
				throw new Exception("wrong number of domain anchor points, need 2, got "+a.length)
			min = a(0).asInstanceOf[Double]
			max = a(1).asInstanceOf[Double]
			this
		}
	}
}

trait Scale {

	def apply(a:Any):Double
	def setDomain(a:Any*):Scale
}