package se.jt

import Geom.Rect
	
class PixelSpace(r:Rect) {
	
	def toX(x:Double) = (r.x + x*r.w).toInt
	def toY(y:Double) = (r.y + r.h - y*r.h).toInt
}