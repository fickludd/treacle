package se.jt

import java.awt.Graphics2D
import java.awt.Color
import Geom.Rect

object PlotCue {
	
	def xAxisHeight[D](g:Graphics2D, scale:Scale[D]) = {
		val fm = g.getFontMetrics
		5 + fm.getHeight
	}

	def xAxis[D](g:Graphics2D, r:Rect, scale:Scale[D], bgColor:Option[Color]) = {
		//g.setColor(Color.WHITE)
		//g.fillRect(r.x, r.y, r.w, r.h)
		
		g.setColor(
			bgColor match {
				case None => Color.BLACK
				case Some(col) =>
					if (col.getBlue + col.getRed + col.getGreen > 383) Color.BLACK
					else Color.WHITE
			})
		//g.drawLine(r.x, r.y, r.x+r.w, r.y)
		
		val fm = g.getFontMetrics
		for ((t, pos) <- scale.ticks) {
			val tx = r.x + (r.w*pos).toInt
			g.drawLine(tx, r.y, tx, r.y + 5)
			val ty = r.y + 5 + fm.getAscent
			g.drawString(t, tx - fm.charsWidth(t.toArray, 0, t.length)/2, ty)
		}
	}

	def xCues[D](g:Graphics2D, r:Rect, scale:Scale[D], col:Color) = {
		g.setColor(col)
		val ps = new PixelSpace(r)
		for ((t, pos) <- scale.ticks) 
			g.drawLine(ps.toX(pos), r.y, ps.toX(pos), r.y + r.h)
	}
	
	def yAxisWidth[D](g:Graphics2D, scale:Scale[D]) = {
		val fm = g.getFontMetrics
		6 + scale.ticks.map(t => fm.charsWidth(t._1.toArray, 0, t._1.length)).max
	}

	def yAxis[D](g:Graphics2D, r:Rect, scale:Scale[D], bgColor:Option[Color]) = {
		//g.setColor(Color.WHITE)
		//g.fillRect(r.x, r.y, r.w, r.h)
		
		g.setColor(
			bgColor match {
				case None => Color.BLACK
				case Some(col) =>
					if (col.getBlue + col.getRed + col.getGreen > 383) Color.BLACK
					else Color.WHITE
			})
		//g.drawLine(r.x+r.w, r.y, r.x+r.w, r.y+r.h)
		
		val fm = g.getFontMetrics
		for ((t, pos) <- scale.ticks) {
			val ty = r.y + (r.h*(1.0-pos)).toInt
			g.drawLine(r.x+r.w-5, ty, r.x+r.w, ty)
			val tx = r.x + r.w - 6 - fm.charsWidth(t.toArray, 0, t.length)
			g.drawString(t, tx, ty + fm.getAscent / 2)
		}
	}

	def yCues[D](g:Graphics2D, r:Rect, scale:Scale[D], col:Color) = {
		g.setColor(col)
		val ps = new PixelSpace(r)
		for ((t, pos) <- scale.ticks) 
			g.drawLine(r.x, ps.toY(pos), r.x + r.w, ps.toY(pos))
	}
}