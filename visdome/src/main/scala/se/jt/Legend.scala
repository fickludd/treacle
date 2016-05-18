package se.jt

import Geom.Rect
import java.awt.Graphics2D
import java.awt.Color

trait Legend[D] {
	def size(g:Graphics2D):(Int, Int)
	def render(g:Graphics2D, r:Rect):Unit
}

object Legend {
	class Color[D](scale:Scale[D], colorSpace:ColorSpace) extends Legend[D] {
		def size(g:Graphics2D):(Int, Int) = {
			val fm = g.getFontMetrics
			(scale.ticks.map(t => fm.charsWidth(t._1.toArray, 0, t._1.length)).max + 10, scale.ticks.length * fm.getHeight)
		}
		
		def render(g:Graphics2D, r:Rect):Unit = {
			val fm = g.getFontMetrics
			if (scale.categorical) {
				for (((str, d), i) <- scale.ticks.zipWithIndex) {
					val y = i*fm.getHeight + r.y
					g.setColor(colorSpace(d))
					g.fillRect(r.x, y + fm.getAscent - 8, 8, 8)
					g.setColor(Color.BLACK)
					g.drawString(str, r.x + 10, y + fm.getAscent)
				}
			} else {
				
			}
		}
	}
}
