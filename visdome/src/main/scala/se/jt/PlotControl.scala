package se.jt



class PlotsControl[D, X, Y](plots:Seq[PlotControl[D, X, Y]]) {
	
	def getControl(px:Int, py:Int):Option[PlotControl[D, X, Y]] = 
		plots.find(_.plotRect.contains(px, py))
}



class PlotControl[D, X, Y](
		val plotRect:Geom.Rect,
		val xScale:Scale[X],
		val yScale:Scale[Y]
) {
	def confineX(px:Int):Int = math.min(plotRect.x + plotRect.w, math.max(px, plotRect.x))
	def confineY(py:Int):Int = math.min(plotRect.y + plotRect.h, math.max(py, plotRect.y))
	def confine(px:Int, py:Int):(Int, Int) = 
		(confineX(px), confineY(py))

	
	def zoomXFilter(px1:Int, px2:Int):(X, Int) => Boolean = {
		val x1 = Util.scale(confineX(px1), plotRect.x, plotRect.x + plotRect.w)
		val x2 = Util.scale(confineX(px2), plotRect.x, plotRect.x + plotRect.w)
		val b1 = xScale.border(x1)
		val b2 = xScale.border(x2)
		(d, i) => 
			b1(d, i) != b2(d, i)
	}

	def zoomYFilter(py1:Int, py2:Int):(Y, Int) => Boolean = {
		val y1 = Util.scale(confineY(py1), plotRect.y, plotRect.y + plotRect.h)
		val y2 = Util.scale(confineY(py2), plotRect.y, plotRect.y + plotRect.h)
		val b1 = yScale.border(y1)
		val b2 = yScale.border(y2)
		(d, i) => 
			b1(d, i) != b2(d, i)
		
	}
}