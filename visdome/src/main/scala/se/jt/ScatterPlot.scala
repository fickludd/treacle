package se.jt

import VisualDimension._
import java.awt.Graphics2D
import java.awt.Color

class ScatterPlot[D](val data:Seq[D]) 
	extends XDim[D, D, D, Seq[D]] 
		with YDim[D, D, D, Seq[D]] 
		with ColorDim[D, D, D, Seq[D]] {

	type Self = ScatterPlot[D]
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
			
		def drawPoints(xs:Seq[Double], ys:Seq[Double], col:Color) = {
		
			g.setColor(col)
			for ((x,y) <- xs.zip(ys)) {
				val px = ps.toX(x)
				val py = ps.toY(y) 
				//g.drawRect(px-1, py-1, 2, 2)
				g.fillOval(px-2, py-2, 4, 4)
				//g.drawLine(px-1, py-1, px+1, py+1)
				//g.drawLine(px+1, py-1, px+1, py-1)
			}
		}
		
		colorScale match {
			case Some(scale) =>
				val groupCol = scale(data)
				val xGroups = Util.split(xs, groupCol)
				val yGroups = Util.split(ys, groupCol)
				for (key <- groupCol.distinct) 
					drawPoints(xGroups(key), yGroups(key), colorSpace(key))
			case None =>
				drawPoints(xs, ys, Color.BLACK)
		}
	}
}