package se.jt

import VisualDimension._
import java.awt.Graphics2D
import java.awt.Color

class LinePlot[D](val data:Seq[D]) 
	extends XDim[D, D, D, Seq[D]] 
		with YDim[D, D, D, Seq[D]] 
		with ColorDim[D, D, D, Seq[D]] {

	type Self = LinePlot[D]
	import Geom.Rect
	
	def checkAndSetup(data:Seq[D]) = {
		if (xScale.isEmpty)
			throw new Exception("x-dimension not defined!")
		
		if (yScale.isEmpty)
			throw new Exception("y-dimension not defined!")
		
		colorScale.foreach(_.setup(this.data))
		
		(xScale.get(data), yScale.get(data), data)
	}
	
	def renderData(
			g:Graphics2D, 
			r:Rect, 
			xScale:Scale[D], 
			yScale:Scale[D],
			data:Seq[D]
	) = {
		val xs = xScale(data)
		val ys = yScale(data)
		val ps = new PixelSpace(r)
		
		def drawLine(xs:Seq[Double], ys:Seq[Double], col:Color) = {
		
			var lx = ps.toX(xs.head)
			var ly = ps.toY(ys.head)
			g.setColor(col)
			for ((x, y) <- xs.tail.zip(ys.tail)) {
				val px = ps.toX(x)
				val py = ps.toY(y)
				g.drawLine(lx, ly, px, py)
				lx = px
				ly = py
			}
		}
		
		colorScale match {
			case Some(scale) =>
				val groupCol = scale(data)
				val xGroups = Util.split(xs, groupCol)
				val yGroups = Util.split(ys, groupCol)
				for (key <- groupCol.distinct) 
					drawLine(xGroups(key), yGroups(key), colorSpace(key))
			case None =>
				drawLine(xs, ys, Color.BLACK)
		}
	}
}