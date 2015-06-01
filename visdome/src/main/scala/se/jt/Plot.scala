package se.jt

import java.awt.Graphics2D
import java.awt.Color

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

object Plot {
	trait FacetScaleType
	case object FreeScales extends FacetScaleType
	case object FreeXScales extends FacetScaleType
	case object FreeYScales extends FacetScaleType
	case object FixedScales extends FacetScaleType
	
	class Conf {
		var xScale = true
		var yScale = true
		var xCue = true
		var yCue = true
		var cueColor = Color.WHITE
		var plotBgColor:Option[Color] = Some(new Color(0xEEEEEE))
		var canvasColor:Option[Color] = Some(Color.WHITE)
		var borderWidth = 5
		
		def withVisibility(xScale:Boolean, yScale:Boolean, xCue:Boolean, yCue:Boolean) = {
			val c = new Conf
			c.xScale = xScale
			c.yScale = yScale
			c.xCue = xCue
			c.yCue = yCue
			c.cueColor = cueColor
			c.plotBgColor = plotBgColor
			c.canvasColor = canvasColor
			c.borderWidth = borderWidth
			c
		}
	}
}

trait Plot[Datum, X, Y, Interrim] {
	
	type Self <: Plot[Datum, X, Y, Interrim]
	val self = this.asInstanceOf[Self]
	
	import Geom.Rect
	import Plot._
	
	val legends = new ArrayBuffer[Legend[Datum]]
	val conf = new Conf
	
	val filters = new Stack[(Datum, Int) => Boolean]
	
	protected var facetA:Option[Stratifier[Datum]] = None
	var facetScaleType:FacetScaleType = FreeScales
	def facetWrap[T](
			f:Datum => T, 
			scaleType:FacetScaleType = FreeScales
		)(
			implicit s:Stratifier[T]
	):Self = {
		facetA = Some(new Stratifier.Wrapper(f, s))
		facetScaleType = scaleType
		self
	}
	
	def data:Seq[Datum]
	def checkAndSetup(data:Seq[Datum]):(Scale[X], Scale[Y], Interrim) 
	def renderData(
			g:Graphics2D, 
			r:Rect, 
			xScale:Scale[X], 
			yScale:Scale[Y],
			interrim:Interrim
		):Unit
	def render(g:Graphics2D, fullRect:Rect):PlotsControl[Datum, X, Y] = {
		
		val filtered = 
			for {
				i <- 0 until data.length
				if filters.forall(f => f(data(i), i))
			} yield data(i)
		
		for (c <- conf.canvasColor) {
			g.setColor(c)
			g.fillRect(fullRect.x, fullRect.y, fullRect.w, fullRect.h)
		}
		
		val r = fullRect.removeBorder(conf.borderWidth)
		
		val subPlots = 
			facetA match {
				case Some(stratA) =>
					val facetAGroups = stratA(filtered)
					for (
						i <- 0 until facetAGroups.length
					) yield checkAndSetup(selectData(filtered, facetAGroups, i))
					
				case None =>
					List(checkAndSetup(filtered))
			}
		
		val lSizes = legends.map(_.size(g))
		val lw = if (lSizes.isEmpty) 0 else lSizes.map(_._1).max
		val plotsRect = Rect(r.x, r.y, r.w-lw, r.h)
		val subPlotRects = plotsRect.niceSplit(subPlots.length)
		
		val plotControls =
			facetScaleType match {
				case FreeScales =>
					for (i <- 0 until subPlots.length) yield {
						val (_xScale, _yScale, interrim) = subPlots(i)
						val plotRect = renderPlotAxis(g, subPlotRects(i), _xScale, _yScale, interrim, conf)
						new PlotControl[Datum, X, Y](plotRect, _xScale, _yScale)
					}
			}
		
		var ly = 0
		for ((l, (w,h)) <- legends.zip(lSizes)) {
			l.render(g, Rect(r.x + r.w - lw, ly, lw, h))
			ly += h
		}
		
		new PlotsControl(plotControls)
	}
	
	def renderPlotAxis(
			g:Graphics2D, 
			r:Rect, 
			_xScale:Scale[X], 
			_yScale:Scale[Y], 
			interrim:Interrim,
			conf:Conf
	):Rect = {
		val xah = if (conf.xScale) PlotCue.xAxisHeight(g, _xScale) else 0 
		val yaw = if (conf.yScale) PlotCue.yAxisWidth(g, _yScale) else 0
		val plotRect = Rect(r.x+yaw+1, r.y, r.w-yaw-1, r.h-xah-1)
		
		for (c <- conf.plotBgColor) {
			g.setColor(c)
			g.fillRect(plotRect.x, plotRect.y, plotRect.w, plotRect.h)
		}
		
		if (conf.xScale) {
			val xar = Rect(r.x+yaw, r.y+r.h-xah, r.w-yaw, xah)
			PlotCue.xAxis(g, xar, _xScale, conf.canvasColor)
		}
		if (conf.yScale) {
			val yar = Rect(r.x, r.y, yaw, r.h-xah)
			PlotCue.yAxis(g, yar, _yScale, conf.canvasColor)
		}
		if (conf.xCue)
			PlotCue.xCues(g, plotRect, _xScale, conf.cueColor)
		if (conf.yCue)
			PlotCue.yCues(g, plotRect, _yScale, conf.cueColor)
		
				
		renderData(g, plotRect, _xScale, _yScale, interrim)
		plotRect
	}
	
	def selectData(data:Seq[Datum], stratification:Stratification, group:Int) = {
		for {
			i <- 0 until data.length
			if stratification.values(i) == group
		} yield data(i)
}
}