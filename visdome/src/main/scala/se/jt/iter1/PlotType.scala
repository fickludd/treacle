package se.jt.iter1

import java.awt.Graphics2D

trait PlotType {

	import Geom._
	
	def isRenderable(plot:Plot):Boolean
	def render(g:Graphics2D, plot:Plot, r:Rect):Unit
}