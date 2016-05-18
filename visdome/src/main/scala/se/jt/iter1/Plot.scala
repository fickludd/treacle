package se.jt.iter1

import java.awt.Graphics2D

object Plot {
	
	def defaultIntScale(xs:Iterable[Any]):Scale = 
		xs.head match {
			case d:Double => 
				val ds = xs.asInstanceOf[Iterable[Double]]
				new ContScale.LinScale(ds.min, ds.max)
		}
}

class Plot(val dp:DataProvider) {

	import Plot._
	import Geom._
	
	var _x:Option[String] = None
	var _y:Option[String] = None
	var _color:Option[String] = None
	
	def x(col:String):Plot = {_x = Some(col); this}
	def y(col:String):Plot = {_y = Some(col); this}
	def color(col:String):Plot = {_color = Some(col); this}
	
	var userXScale:Option[Scale] = None
	var userYScale:Option[Scale] = None
	def xScale(xs:Iterable[Any]):Scale = userXScale.getOrElse(defaultIntScale(xs)) 
	def yScale(ys:Iterable[Any]):Scale = userYScale.getOrElse(defaultIntScale(ys))
	
	var xAxis:Axis = new ContAxis
	var yAxis:Axis = new ContAxis
	
	var _geom:PlotType = _
	def geom(pt:PlotType):Plot = {_geom = pt; this}
	
	def render(g:Graphics2D, r:Rect) = {
		xAxis.scale(xScale(dp(_x.get)))
		val xah = xAxis.thickness
		yAxis.scale(yScale(dp(_y.get)))
		val yaw = yAxis.thickness
		
		val xar = Rect(r.x+yaw, r.y+r.h-xah, r.w-yaw, xah)
		val yar = Rect(r.x, r.y, yaw, r.h-xah)
		val plotRect = Rect(r.x+yaw, r.y, r.w-yaw, r.h-xah)
		xAxis.render(g, xar)
		yAxis.render(g, yar)
		
		_geom.render(g, this, plotRect)
	}
	/*
	var _x:DataProvider => Double = _
	var _y:DataProvider => Double = _
	var _color:DataProvider => Color = _
	var _fill:DataProvider => Color = _
	var _group:DataProvider => String = _
	var _alpha:DataProvider => Double = _
	def x(f:DataProvider => Double):Plot = {_x = f; this}
	def y(f:DataProvider => Double):Plot = {_y = f; this}
	def color(f:DataProvider => Color):Plot = {_color = f; this}
	def fill(f:DataProvider => Color):Plot = {_fill = f; this}
	def group(f:DataProvider => String):Plot = {_group = f; this}
	def alpha(f:DataProvider => Double):Plot = {_alpha = f; this}
	* 
	*/
}