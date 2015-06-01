package se.jt

object VisualDimension {

	trait XDim[D, X, Y, I] extends Plot[D, X, Y, I] {
		var xScale:Option[GetScale[D]] = None
		var xStratifier:Option[Stratifier[D]] = None
		def x[T](f:D => T)(implicit g:GetScale[T], s:Stratifier[T]):Self = {
			xScale = Some(new Scale.GetWrapper(f, g))
			xStratifier = Some(new Stratifier.Wrapper(f, s))
			self
		}
	}

	trait YDim[D, X, Y, I] extends Plot[D, X, Y, I] {
		var yScale:Option[GetScale[D]] = None
		var yStratifier:Option[Stratifier[D]] = None
		def y[T](f:D => T)(implicit g:GetScale[T], s:Stratifier[T]):Self = {
			yScale = Some(new Scale.GetWrapper(f, g))
			yStratifier = Some(new Stratifier.Wrapper(f, s))
			self
		}
	}

	trait ColorDim[D, X, Y, I] extends Plot[D, X, Y, I] {
		var colorScale:Option[Scale[D]] = None
		var colorSpace:ColorSpace = _
		var colorStratifier:Option[Stratifier[D]] = None
		def color[T](f:D => T)(implicit g:GetScale[T], s:Stratifier[T]):Self = {
			colorScale = Some(new Scale.Wrapper(f, g()))
			colorSpace = 
				if (colorScale.get.categorical) ColorBrewer.Set1
				else ColorBrewer.BrBG
			colorStratifier = Some(new Stratifier.Wrapper(f, s))
			legends += new Legend.Color(colorScale.get, colorSpace)
			self
		}
	}

	trait SizeDim[D, X, Y, I] extends Plot[D, X, Y, I] {
		var sizeScale:Option[GetScale[D]] = None
		var sizeStratifier:Option[Stratifier[D]] = None
		def size[T](f:D => T)(implicit g:GetScale[T], s:Stratifier[T]):Self = {
			sizeScale = Some(new Scale.GetWrapper(f, g))
			sizeStratifier = Some(new Stratifier.Wrapper(f, s))
			self
		}
	}
	
	trait AggrDim[D, X, Y, I] extends Plot[D, X, Y, I] {
		var aggrFunction:Option[D => Double] = None
		def aggr(f:D => Double):Self = {
			aggrFunction = Some(f)
			self
		}
	}
}