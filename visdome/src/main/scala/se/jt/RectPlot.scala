package se.jt

import VisualDimension._
import java.awt.Graphics2D
import java.awt.Color

class RectPlot[D](val data:Seq[D])
	extends X2Dim[D, D, D, Seq[D]]
		with Y2Dim[D, D, D, Seq[D]]
		with ColorDim[D, D, D, Seq[D]] {

	type Self = RectPlot[D]
	import Geom.Rect
	
	var x2Scale:Scale[D] = _
	var y2Scale:Scale[D] = _
	
	def checkAndSetup(data:Seq[D]) = {
		if (xScale.isEmpty)
			throw new Exception("x-dimension not defined!")
		
		if (yScale.isEmpty)
			throw new Exception("y-dimension not defined!")
		
		colorScale.foreach(_.setup(this.data))
		
		val xScales = xScale.get(data)
		val yScales = yScale.get(data)
		
		x2Scale = xScales(1)
		y2Scale = yScales(1)
		
		(xScales.head, yScales.head, data)
	}
	
	def renderData(
			g:Graphics2D, 
			r:Rect, 
			x1Scale:Scale[D], 
			y1Scale:Scale[D],
			data:Seq[D]
	) = {
		val x1s = x1Scale(data)
		val y1s = y1Scale(data)
		val x2s = x2Scale(data)
		val y2s = y2Scale(data)
		val ps = new PixelSpace(r)
		
		def drawRect(x1s:Seq[Double], y1s:Seq[Double], x2s:Seq[Double], y2s:Seq[Double], col:Color) = {
			
			g.setColor(col)
			for (i <- 0 until x1s.length) {
				val px1 = ps.toX(x1s(i))
				val py1 = ps.toY(y1s(i))
				val px2 = ps.toX(x2s(i))
				val py2 = ps.toY(y2s(i))
				g.drawRect(
						math.min(px1, px2), 
						math.min(py1, py2), 
						math.abs(px1-px2), 
						math.abs(py1-py2))
			}
		}
		
		colorScale match {
			case Some(scale) =>
				val groupCol = scale(data)
				val x1Groups = Util.split(x1s, groupCol)
				val y1Groups = Util.split(y1s, groupCol)
				val x2Groups = Util.split(x2s, groupCol)
				val y2Groups = Util.split(y2s, groupCol)
				for (key <- groupCol.distinct) 
					drawRect(x1Groups(key), y1Groups(key), x2Groups(key), y2Groups(key), colorSpace(key))
			case None =>
				drawRect(x1s, y1s, x2s, y2s, Color.BLACK)
		}
	}
}