package se.jt.iter1

import java.awt.Graphics2D

trait Axis {

	def scale(s:Scale):Unit
	def ticks(n:Int):Axis
	
	def thickness:Int
	def render(g:Graphics2D, r:Geom.Rect):Unit
}