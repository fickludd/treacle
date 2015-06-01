package se.jt

object Geom {
	
	case class Rect(x:Int, y:Int, w:Int, h:Int) {
		def niceSplit(n:Int) = {
			val nCol = 
				if (n < 4) n
				else math.ceil(math.sqrt(n)).toInt
				
			val nRow = if (n % nCol == 0) n / nCol else n / nCol + 1
			val xBorders = (0 to nCol).map(i => (i * w) / nCol)
			val yBorders = (0 to nRow).map(i => (i * h) / nRow)
			for (i <- 0 until n) yield {
				val ic = i % nCol
				val ir = i / nCol
				Rect(xBorders(ic), yBorders(ir), 
						xBorders(ic+1) - xBorders(ic),
						yBorders(ir+1) - yBorders(ir))
			}
		}
		
		def removeBorder(b:Int) =
			Rect(x+b, y+b, w-2*b, h-2*b)
			
		def contains(px:Int, py:Int) =
			px >= x && px < x + w && py >= y && py < y + h
	}

}