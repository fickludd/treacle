package se.jt

import VisualDimension._
import java.awt.Graphics2D
import java.awt.Color

object Histogram {
	class Interrim[D](
			data:Seq[D], 
			xStrat:Stratifier[D],
			colorStratifier:Option[Stratifier[D]]
	) {
		val xGroups:Stratification = xStrat(data)
		val colGroups:Option[Stratification] = 
			colorStratifier.map(colStrat => colStrat(data))
		val ints:LevelCount = 
			colGroups match {
				case Some(cg) =>
					JoinCount(xGroups, cg)
				case None => xGroups
			}
		
		def setup(barLayout:BarLayout) = 
			(new StratifiedScale[D](xGroups), 
			barLayout match {
				case Fill =>
					new Scale.IntScale().setup(Array(0, 100))
				case Dodge =>
					new Scale.IntScale().setup(ints.counts :+ 0)
				case Stack =>
					new Scale.IntScale().setup(xGroups.counts :+ 0)
			}, 
			this)
		
	}
	
	trait BarLayout
	case object Stack extends BarLayout
	case object Dodge extends BarLayout
	case object Fill extends BarLayout
}

class Histogram[D](val data:Seq[D]) 
	extends XDim[D, D, Int, Histogram.Interrim[D]]
		with ColorDim[D, D, Int, Histogram.Interrim[D]] {
	
	type Self = Histogram[D]
	import Geom.Rect
	import Histogram._

	val nBins = 10
	
	protected var _barLayout:BarLayout = Stack
	def barLayout(layout:BarLayout):Self = {
		_barLayout = layout
		self
	}
	
	def makeBins(nBins:Int) =
		(0 until nBins).map(i => (i.toDouble/nBins, (i.toDouble+1)/nBins))
	
	def checkAndSetup(data:Seq[D]) = {
		colorScale.foreach(_.setup(this.data))
		
		xStratifier match {
			case Some(xStrat) =>
				val interrim = new Histogram.Interrim(data, xStrat, colorStratifier)
				interrim.setup(_barLayout)
			case None =>
				throw new Exception("x-dimension not defined!")
		}
	}
	
	
	def renderData(
			g:Graphics2D, 
			r:Rect, 
			xScale:Scale[D], 
			yScale:Scale[Int],
			interrim:Histogram.Interrim[D]
	) = {
		val ps = new PixelSpace(r)
		
		def drawRects(
				levelCount:LevelCount,
				colors:Seq[Color],
				nCol:Int
		) = {
			val nX = interrim.xGroups.length
			val xBins = makeBins(nX)
			val colBins = makeBins(nCol)
			for ((key, count) <- levelCount.keyCount) {
				val colId = if (nCol > 1) key(1) else 0
				g.setColor(colors(colId))
				val (x0, x1) = xBins(key(0))
				val sums = 0 +: levelCount.keyCount.filter(_._1(0) == key(0)).sortBy(_._1(1)).map(_._2).toArray
				for (i <- 1 until sums.length) 
					sums(i) = sums(i) + sums(i-1)
				_barLayout match {
					case Stack =>
						val px0 = ps.toX(x0)
						val px1 = ps.toX(x1)
						val py0 = ps.toY(yScale(sums(colId), 0))
						val py1 = ps.toY(yScale(sums(colId+1), 0))
						g.fillRect(px0, py1, 
								math.abs(px1 - px0)-1,
								math.abs(py0 - py1)
							)
					
					case Dodge =>
						val (c0, c1) = colBins(colId)
						val px0 = ps.toX(x0 + (x1-x0)*c0)
						val px1 = ps.toX(x0 + (x1-x0)*c1)
						val py0 = ps.toY(yScale(0, 0))
						val py1 = ps.toY(yScale(count, 0))
						g.fillRect(px0, py1, 
								math.abs(px1 - px0)-1, 
								math.abs(py0 - py1)
							)
							
					case Fill =>
						val px0 = ps.toX(x0)
						val px1 = ps.toX(x1)
						val py0 = ps.toY(yScale((sums(colId)*100) / sums.last, 0))
						val py1 = ps.toY(yScale((sums(colId+1)*100) / sums.last, 0))
						g.fillRect(px0, py1, 
								math.abs(px1 - px0)-1,
								math.abs(py0 - py1)
							)
						
				}
			}
		}
		
		interrim.colGroups match {
			case Some(cgs) =>
				drawRects(interrim.ints, colorScale.get.ticks.map(t => colorSpace(t._2)), cgs.length)
			case None =>
				drawRects(interrim.ints, ColorBrewer.Set1.colors, 1)
		}
	}
}