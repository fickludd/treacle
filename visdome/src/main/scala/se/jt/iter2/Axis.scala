package se.jt.iter2

import java.awt.Graphics2D
import java.awt.Color
import Geom.Rect

object Axis {
	
	def xAxisHeight[D](g:Graphics2D, scale:Scale[D]) = {
		val fm = g.getFontMetrics
		5 + fm.getHeight
	}

	def xAxis[D](g:Graphics2D, r:Rect, scale:Scale[D]) = {
		g.setColor(Util.randColor(0.3))
		g.fillRect(r.x, r.y, r.w, r.h)
		
		g.setColor(Color.BLACK)
		g.drawLine(r.x, r.y, r.x+r.w, r.y)
		
		val fm = g.getFontMetrics
		for ((t, pos) <- scale.ticks) {
			val tx = r.x + (r.w*pos).toInt
			g.drawLine(tx, r.y, tx, r.y + 5)
			val ty = r.y + 5 + fm.getAscent
			g.drawString(t, tx - fm.charsWidth(t.toArray, 0, t.length)/2, ty)
		}
	}
	
	def yAxisWidth[D](g:Graphics2D, scale:Scale[D]) = {
		val fm = g.getFontMetrics
		5 + scale.ticks.map(t => fm.charsWidth(t._1.toArray, 0, t._1.length)).max
	}

	def yAxis[D](g:Graphics2D, r:Rect, scale:Scale[D]) = {
		g.setColor(Util.randColor(0.3))
		g.fillRect(r.x, r.y, r.w, r.h)
		
		g.setColor(Color.BLACK)
		g.drawLine(r.x+r.w, r.y, r.x+r.w, r.y+r.h)
		
		val fm = g.getFontMetrics
		for ((t, pos) <- scale.ticks) {
			val ty = r.y + (r.h*(1.0-pos)).toInt
			g.drawLine(r.x+r.w-5, ty, r.x+r.w, ty)
			val tx = r.x + r.w - 5 - fm.charsWidth(t.toArray, 0, t.length)
			g.drawString(t, tx, ty + fm.getAscent / 2)
		}
	}
}