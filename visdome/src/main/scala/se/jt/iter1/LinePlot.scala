package se.jt.iter1

import java.awt.Graphics2D
import java.awt.Color

class LinePlot extends PlotType {
	
	import Geom._

	def isRenderable(plot:Plot):Boolean =
		plot._x.isDefined && plot._y.isDefined
	
	def render(g:Graphics2D, plot:Plot, r:Rect):Unit = {
		val xs = plot.dp(plot._x.get)
		val ys = plot.dp(plot._y.get)
		
		val groups = 
			plot._color.map(col => plot.dp(col).toSet)
		
		val xScale = plot.xScale(xs)
		val yScale = plot.xScale(ys)
		
		def toScreenSpaceX(x:Double) = (r.x + x*r.w).toInt
		def toScreenSpaceY(y:Double) = (r.y + y*r.h).toInt
		
		def drawLine(xs:Iterable[Any], ys:Iterable[Any], col:Color) = {
		
			var lx = xs.head
			var ly = ys.head
			g.setColor(col)
			for ((x, y) <- xs.zip(ys)) {
				g.drawLine(
						toScreenSpaceX(xScale(lx)), 
						toScreenSpaceY(yScale(ly)), 
						toScreenSpaceX(xScale(x)), 
						toScreenSpaceY(yScale(y))
					)
				lx = x
				ly = y
			}
		}
		
		groups match {
			case Some(groupSet) =>
				val groupCol = plot.dp(plot._color.get)
				val xGroups = Util.split(xs, groupCol)
				val yGroups = Util.split(ys, groupCol)
				for (key <- groupCol.toSeq.distinct) 
					drawLine(xGroups(key), yGroups(key), Util.randColor(0.5))
			case None =>
				drawLine(xs, ys, Color.BLACK)
		}
		
		
	}	
}