package se.jt.iter2

import java.awt.Graphics2D
import java.awt.Color

class LinePlot extends PlotType {
	
import Geom._
	
	def checkAndSetup[D](plot:Plot[D]) = {
		if (plot.xScale.isEmpty) 
			throw new Exception("variable for x-axis not defined!")
		else if (plot.yScale.isEmpty) 
			throw new Exception("variable for y-axis not defined!")
		
		plot.xScale.get.setup(plot.data)
		plot.yScale.get.setup(plot.data)
	}
	
	def render[D](g:Graphics2D, plot:Plot[D], r:Rect):Unit = {
		val xs = plot.data.map(plot.xScale.get(_))
		val ys = plot.data.map(plot.yScale.get(_))
		
		val groups = 
			plot.colorScale.map(scale => plot.data.map(scale(_)).toSet)
		
		def toScreenSpaceX(x:Double) = (r.x + x*r.w).toInt
		def toScreenSpaceY(y:Double) = (r.y + r.h - y*r.h).toInt
		
		def drawLine(xs:Seq[Double], ys:Seq[Double], col:Color) = {
		
			var lx = xs.head
			var ly = ys.head
			g.setColor(col)
			for ((x, y) <- xs.zip(ys)) {
				g.drawLine(
						toScreenSpaceX(lx), 
						toScreenSpaceY(ly), 
						toScreenSpaceX(x), 
						toScreenSpaceY(y)
					)
				lx = x
				ly = y
			}
		}
		
		groups match {
			case Some(groupSet) =>
				val groupCol = plot.data.map(plot.colorScale.get(_))
				val xGroups = Util.split(xs, groupCol)
				val yGroups = Util.split(ys, groupCol)
				for (key <- groupSet) 
					drawLine(xGroups(key), yGroups(key), Util.randColor(0.5))
			case None =>
				drawLine(xs, ys, Color.BLACK)
		}
		
		
	}	
}