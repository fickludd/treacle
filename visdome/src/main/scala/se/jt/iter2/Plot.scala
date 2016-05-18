package se.jt.iter2

import java.awt.Graphics2D

import se.jt.iter2.Geom.Rect;
import Geom.Rect

class Plot[D](val data:Seq[D]) {
	
	def x[T](f:D => T)(implicit g:GetScale[T]) = {
		//_x = f
		xScale = Some(new Scale.ScaleWrapper(f, g()))
		this
	}
	
	def y[T](f:D => T)(implicit g:GetScale[T]) = {
		//_x = f
		yScale = Some(new Scale.ScaleWrapper(f, g()))
		this
	}
	
	def color[T](f:D => T)(implicit g:GetScale[T]) = {
		//_x = f
		colorScale = Some(new Scale.ScaleWrapper(f, g()))
		this
	}
	
	var _geom:PlotType = _
	def geom(pt:PlotType) = {
		_geom = pt
		this
	}
	
	var xScale:Option[Scale[D]] = None
	var yScale:Option[Scale[D]] = None
	var colorScale:Option[Scale[D]] = None
	
	def render(g:Graphics2D, r:Rect) = {
		_geom.checkAndSetup(this)
		
		for {
			_xScale <- xScale
			_yScale <- yScale
		} {
			val xah = Axis.xAxisHeight(g, _xScale) 
			val yaw = Axis.yAxisWidth(g, _yScale)
			
			val xar = Rect(r.x+yaw, r.y+r.h-xah, r.w-yaw, xah)
			val yar = Rect(r.x, r.y, yaw, r.h-xah)
			val plotRect = Rect(r.x+yaw, r.y, r.w-yaw, r.h-xah)
			
			if (_xScale.visible)
				Axis.xAxis(g, xar, _xScale)
			if (_yScale.visible)
				Axis.yAxis(g, yar, _yScale)
			
			if (colorScale.isDefined) 
				colorScale.get.setup(data)
				
			_geom.render(g, this, plotRect)
		}
	}

}