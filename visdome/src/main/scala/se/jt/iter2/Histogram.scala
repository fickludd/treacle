package se.jt.iter2

import java.awt.Graphics2D
import java.awt.Color

class Histogram extends PlotType {

	import Geom._
	
	val nBins = 10
	val d = 1.0 / nBins
	val bins = (0 until nBins).map(i => (i*d, (i+1)*d))
	
	var ys:Seq[Int] = _ 
	
	def checkAndSetup[D](plot:Plot[D]) = {
		if (plot.xScale.isEmpty) 
			throw new Exception("variable for x-axis not defined!")
		
		plot.xScale.get.setup(plot.data)
		val xs = plot.data.map(plot.xScale.get(_))
		ys = for ((min, max) <- bins) yield {
			xs.count(d => d >= min && d < max)
		}
		plot.yScale = Some(new Scale.FixScale(0, ys.max))
	}
		
		
	def render[D](g:Graphics2D, plot:Plot[D], r:Rect):Unit = {
		val groups = 
			plot.colorScale.map(scale => plot.data.map(scale(_)).toSet)
		
		def toScreenSpaceX(x:Double) = (r.x + x*r.w).toInt
		def toScreenSpaceY(y:Int) = (r.y + r.h - (y.toDouble/ys.max)*r.h).toInt
		
		
		
		def drawRects(bins:Seq[(Double, Double)], ys:Seq[Int], col:Color) = {
			g.setColor(col)
			for (((b0, b1),y) <- bins.zip(ys)) {
				val barWidth = toScreenSpaceX(b1) - toScreenSpaceX(b0) - 1
				g.fillRect(
						toScreenSpaceX(b0), 
						toScreenSpaceY(y), 
						barWidth, 
						((y.toDouble/ys.max)*r.h).toInt
					)
			}
		}
		
		groups match {
			case Some(groupSet) =>
				val groupCol = plot.data.map(plot.colorScale.get(_))
				val xGroups = Util.split(bins, groupCol)
				val yGroups = Util.split(ys, groupCol)
				for (key <- groupSet) 
					drawRects(xGroups(key), yGroups(key), Util.randColor(0.5))
			case None =>
				drawRects(bins, ys, Color.BLACK)
		}
	}
}