package se.jt.iter2

import java.awt.Graphics2D

trait PlotType {

	import Geom._
	
	def checkAndSetup[D](plot:Plot[D]):Unit
	def render[D](g:Graphics2D, plot:Plot[D], r:Rect):Unit
}