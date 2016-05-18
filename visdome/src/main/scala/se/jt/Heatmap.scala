package se.jt

import VisualDimension._
import java.awt.Graphics2D
import java.awt.Color
import java.awt.image.BufferedImage

class HeatMap[D](
		val data:Seq[D],
		val colorTransform:Double => Double = d => d
) extends XDim[D, D, D, Seq[D]] 
		with YDim[D, D, D, Seq[D]] 
		with AggrDim[D, D, D, Seq[D]]  {

	type Self = HeatMap[D]
	import Geom.Rect
	
	def color = aggr _
	
	conf.plotBgColor = Some(Color.DARK_GRAY)
	conf.canvasColor = Some(Color.BLACK)
	conf.cueColor = Color.GRAY
	
	def checkAndSetup(data:Seq[D]) = {
		if (xScale.isEmpty)
			throw new Exception("x-dimension not defined!")
		
		if (yScale.isEmpty)
			throw new Exception("y-dimension not defined!")
		
		if (aggrFunction.isEmpty)
			throw new Exception("aggregation-dimension / color-dimension not defined!")
		
		
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
		val cs = data.map(aggrFunction.get(_))
		
		val ps = new PixelSpace(Rect(0,0,r.w, r.h))
		
		val buffer = new Array[Array[Double]](r.w)
		for (i <- 0 until r.w) buffer(i) = new Array(r.h)
		val bufferImage = Util.getClearBufferImage(r.w, r.h)
		
		/*
		def updateBuffer(mz:Double, mzw:Double, index:Int, int:Double) = {
			val x1 = tox(index)
			val x2 = tox(index + 1)
			val y2 = toy(mz - mzw)
			val y1 = toy(mz + mzw)
			val w = (x2-x1)
			val h = (y2-y1)
			for {
				x <- x1 until math.min(pw, x1+math.max(w,1))
				y <- y1 until math.min(ph, y1+math.max(h,1))
			} {
				buffer(x)(y) += int
			}
		}*/
		
		def updateBuffer(x1:Double, x2:Double, y1:Double, y2:Double, c:Double) = {
			val px1 = ps.toX(x1)
			val px2 = ps.toX(x2)
			val py1 = ps.toY(y2) // switch because y-axis is reversed
			val py2 = ps.toY(y1)
			for {
				x <- px1 until math.min(r.w, math.max(px1+1, px2))
				y <- py1 until math.min(r.h, math.max(py1+1, py2))
			} {
				if (x >= 0 && x < r.w && y >= 0 && y < r.h)
					buffer(x)(y) += c
			}
		}
		
		if (xScale.categorical) {
			val colXs = xs.distinct.sorted
			val (x1, x2) = makeBins(colXs)
			val cols = Util.split(0 until ys.length, xs)
			for (i <- 0 until colXs.length) {
				val sorted = cols(colXs(i)).sortBy(ys)
				val (y1, y2) = makeBins(sorted.map(ys))
				for (j <- 0 until sorted.length) 
					updateBuffer(x1(i), x2(i), y1(j), y2(j), cs(sorted(j)))
			}
		} else if (yScale.categorical) {
			val rowYs = ys.distinct.sorted
			val (y1, y2) = makeBins(rowYs)
			val rows = Util.split(0 until xs.length, ys)
			for (i <- 0 until rowYs.length) {
				val sorted = rows(rowYs(i)).sortBy(xs)
				val (x1, x2) = makeBins(sorted.map(xs))
				for (j <- 0 until sorted.length) 
					updateBuffer(x1(i), x2(i), y1(j), y2(j), cs(sorted(j)))
			}
		} else {
			val dx = 0.05
			val dy = 0.05
			for (i <- 0 until ys.length) {
				val x = xs(i)
				val y = ys(i)
				val c = cs(i)
				updateBuffer(x-dx, x+dx, y-dy, y+dy, c)
			}
		}
		
		
		val bufferMin = colorTransform(buffer.map(_.min).min)
		val bufferMax = colorTransform(buffer.map(_.max).max)
		val colorSpace = ColorBrewer.BrBG
		for {
			x <- 0 until r.w
			y <- 0 until r.h
		} {
			val count = buffer(x)(y) 
			if (count > 0) {
				val k = Util.scale(colorTransform(count), bufferMin, bufferMax)
				bufferImage.setRGB(x, y, colorSpace(k).getRGB)
			}
			
		}
		g.drawImage(bufferImage, null, r.x, r.y)
	}
	
	
	def makeBins(xs:Seq[Double]):(Seq[Double], Seq[Double]) = {
		val l = xs.length
		val x1 = new Array[Double](l)
		val x2 = new Array[Double](l)
		for (i <- 1 until l) {
			val w = xs(i) - xs(i-1)
			x1(i-1) = xs(i-1) - w/2
			x2(i-1) = xs(i-1) + w/2
		}
		x1(l-1) = 2*x1(l-2) - x1(l-3)
		x2(l-1) = 2*x2(l-2) - x2(l-3)
		(x1, x2)
	}
}