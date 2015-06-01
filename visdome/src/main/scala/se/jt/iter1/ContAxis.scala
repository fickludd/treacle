package se.jt.iter1

import java.awt.Graphics2D

class ContAxis extends Axis {

	import Geom._
	
	def ticks(n:Int):Axis = {this}
	
	var _scale:Scale = _
	def scale(s:Scale):Unit =
		_scale = s
	
	def thickness:Int = 20
	def render(g:Graphics2D, r:Rect):Unit = {
		g.setColor(Util.randColor(0.2))
		g.fillRect(r.x, r.y, r.w, r.h)
		
		
	}
	
}