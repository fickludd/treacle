package se.jt

import java.awt.Color
import java.awt.Graphics2D
import scala.util.Random
import java.awt.image.BufferedImage

object Util {
	
	case class ImgControl[D, X, Y](img:BufferedImage, control:PlotsControl[D, X, Y])
	
	def split[T, G](x:Iterable[T], g:Iterable[G]):Map[G, Iterable[T]] = {
		val keys = g.toSet
		(for (key <- keys) yield (
			key -> x.zip(g).filter(_._2 == key).map(_._1)
		)).toMap
	}
	
	def split[T, G](x:Seq[T], g:Seq[G]):Map[G, Seq[T]] = {
		val keys = g.toSet
		(for (key <- keys) yield (
			key -> x.zip(g).filter(_._2 == key).map(_._1)
		)).toMap
	}
	
	
	def randColor(k:Double) = 
		new Color(
				(math.min(1.0, 0.4 + Random.nextDouble*k)).toFloat, 
				(math.min(1.0, 0.4 + Random.nextDouble*k)).toFloat, 
				(math.min(1.0, 0.4 + Random.nextDouble*k)).toFloat
			)
	
	def scale(x:Double, min:Double, max:Double) =
		(x - min) / (max - min)
		
	def getBufferImage(w:Int, h:Int, col:Color) = {
		val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
		
		val g = img.createGraphics
		
		g.setColor(col)
		g.fillRect(0, 0, w, h)
		g.dispose
		
		img
	}
		
	def getClearBufferImage(w:Int, h:Int) = {
		val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
		img
	}
		
	def drawToBuffer(w:Int, h:Int, f:Graphics2D => Unit) = {
		val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
		
		val g = img.createGraphics
		
		f(g)
		g.dispose
		
		img
	}
		
	def drawToBuffer[D,X,Y,I](w:Int, h:Int, plot:Plot[D,X,Y,I]) = {
		val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
		
		val g = img.createGraphics
		val plotsControl = plot.render(g, Geom.Rect(0,0,w,h))
		g.dispose
		
		ImgControl(img, plotsControl)
	}
	
	
}