package se.jt

import java.awt.Color


object Colors {
	def interpolate(c1:Color, c2:Color, k:Double) = { 
		val a1 = c1.getRGBColorComponents(null)
		val a2 = c2.getRGBColorComponents(null)
		new Color(
				(a1(0) * (1-k) + a2(0) * k).toFloat,
				(a1(1) * (1-k) + a2(1) * k).toFloat,
				(a1(2) * (1-k) + a2(2) * k).toFloat)
	}
}

trait ColorSpace extends Function1[Double, Color] {
	def apply(d:Double):Color
}

class CategoricalColorSpace(
		val colors:Array[Color]
) extends ColorSpace {
	def apply(d:Double):Color =
			colors(math.min(colors.length-1, (d * colors.length).toInt))
}

class GradientColorSpace(
		val colors:Array[Color],
		val anchors:Array[Double]
) extends ColorSpace {
	def apply(d:Double):Color = {
		for (i <- 1 until colors.length)
			if (d >= anchors(i-1) && d <= anchors(i))
				return Colors.interpolate(
						colors(i-1), 
						colors(i), 
						(d-anchors(i-1)) / (anchors(i) - anchors(i-1))
					)
		throw new Exception("value '%f' not in color space range".format(d))
	}
		
}

object ColorBrewer {
	val Set1 = new CategoricalColorSpace(Array(
				new Color(0xe41a1c), new Color(0x377eb8), new Color(0x4daf4a), 
				new Color(0x984ea3), new Color(0xff7f00), new Color(0x999999), 
				new Color(0xf781bf), new Color(0xffff33), new Color(0xa65628)))
	
	
	val BrBG = new GradientColorSpace(
			Array(
				new Color(0x543005), new Color(0xc510a), new Color(0xbf812d), 
				new Color(0xdfc27d), new Color(0xf6e8c3), new Color(0xf5f5f5), 
				new Color(0xc7eae5), new Color(0x80cdc1), new Color(0x35978f), 
				new Color(0x01665e), new Color(0x003c30)),
			(0 until 11).map(_ / 10.0).toArray
		)
			
}