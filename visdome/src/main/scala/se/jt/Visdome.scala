package se.jt

import scala.swing._
import scala.swing.event._

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

import java.awt.Color

object Visdome extends SimpleSwingApplication {
	
	import Scale._
	import Stratifier._
	case class Data(t:Double, t2:Double, position:Double, pos2:Double, group:String)
	case class Point(x:Int, y:Double)
	
	val ts = (0 until 100).map(_ / 10.0)
	val y1 = ts.take(91).map(t => Data(t, t+0.1, math.sin(t)*1.3, math.sin(t)*1.3+0.1, "y1"))
	val y2 = ts.take(99).map(t => Data(t, t+0.15, math.sin(2 * t), math.sin(2 * t)+0.05, "y2"))
	val y3 = ts.take(86).map(t => Data(t, t+0.05, math.sin(1+t)*0.94, math.sin(1+t)*0.94+0.05, "y3"))
	
	val data = y1 ++ y2 ++ y3
	
	val points = (0 until 10000).map(i => Point(Random.nextInt(5), Random.nextDouble))
	
	val SIZE = new java.awt.Dimension(1000, 1000)
	
	case class Marker[D, X, Y](ctrl:PlotControl[D, X, Y], px:Int, py:Int)
	
	
	
	object RectPlotComp extends Component {
		
		preferredSize = SIZE
		
		val plot = new RectPlot(data).x(_.t, _.t2).y(_.position, _.pos2).color(_.group)
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			plot.render(g, Geom.Rect(0, 0, size.width, size.height))
		}
	}
	
	object OverlayComp extends Component {
		
		preferredSize = SIZE
		
		val plot = new RectPlot(data).x(_.t, _.t2).y(_.position, _.pos2).color(_.group)
		plot.overlays += new LinePlot(data).x(_.t).y(_.position).color(_.group)
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			plot.render(g, Geom.Rect(0, 0, size.width, size.height))
		}
	}
	
	object LinePlotComp extends Component {
		
		preferredSize = SIZE
		
		val plot = new LinePlot(data).x(_.t).y(_.position).color(_.group)
		var control:PlotsControl[Data, Data, Data] = _
		var m1:Option[Marker[Data, Data, Data]] = None
		var m2:Option[Marker[Data, Data, Data]] = None
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			control = plot.render(g, Geom.Rect(0, 0, size.width, size.height))
			
			g.setColor(Color.BLACK)
			def plotMarker[D, X, Y](m:Option[Marker[D, X, Y]]) =
				for (Marker(ctrl, px, py) <- m) {
					val cx = ctrl.confineX(px)
					val cy0 = ctrl.confineY(0)
					val cy1 = ctrl.confineY(1000000)
					g.drawLine(cx, cy0, cx, cy1)
				}
			plotMarker(m1)
			plotMarker(m2)
		}
		
		listenTo(this.mouse.moves)
		listenTo(this.mouse.clicks)
		
		reactions += {
			case mm:MouseMoved =>
				control.getControl(mm.point.x, mm.point.y) match {
					case Some(ctrl) =>
						m1 = Some(Marker(ctrl, mm.point.x, mm.point.y))
						repaint
						
					case None => println("not in control")
				}
				
			case md:MouseDragged =>
				control.getControl(md.point.x, md.point.y) match {
					case Some(ctrl) =>
						m2 = Some(Marker(ctrl, md.point.x, md.point.y))
						repaint
						
					case None => println("not in control")
				}
				
			case mr:MouseReleased => 
				if (mr.peer.getButton == java.awt.event.MouseEvent.BUTTON1) {
					for {
						Marker(ctrl, px1, _) <- m1
						Marker(_, px2, _) <- m2
					} {
						plot.filters.push(ctrl.zoomXFilter(px1, px2))
						m1 = None
						m2 = None
					}
				} else
					plot.filters.pop
				repaint
		}
	}
	
	object FacetWrapComp extends Component {
		
		preferredSize = SIZE
		
		val plot = new LinePlot(data).x(_.t).y(_.position).facetWrap(_.group).color(_.group)
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			plot.render(g, Geom.Rect(0, 0, size.width, size.height))
		}
	}
	
	object ScatterPlotComp extends Component {
		
		preferredSize = SIZE
		
		val plot = new ScatterPlot(data).x(_.group).y(_.position).color(_.group)
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			plot.render(g, Geom.Rect(0, 0, size.width, size.height))
		}
	}
	
	object Log10ScatterPlotComp extends Component {
		
		preferredSize = SIZE
		
		val plot = new ScatterPlot(TestData.mtcars).x(_.mpg)(log10Scale, doubleStratifier).y(_.hp).color(_.gear)
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			plot.render(g, Geom.Rect(0, 0, size.width, size.height))
		}
	}
	
	object HistogramComp extends Component {
		
		preferredSize = SIZE
		
		val plot = new Histogram(data).x(_.position).color(_.group).barLayout(Histogram.Fill)
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			plot.render(g, Geom.Rect(0, 0, size.width, size.height))
		}
	}
	
	object Histogram2Comp extends Component {
		
		preferredSize = SIZE
		
		val plot = new Histogram(data).x(_.group)
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			plot.render(g, Geom.Rect(0, 0, size.width, size.height))
		}
	}
	
	object HeatMapComp extends Component {
		
		preferredSize = SIZE
		
		val plot = new HeatMap(points).x(_.x).y(_.y).color(_ => 1.0)
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			plot.render(g, Geom.Rect(0, 0, size.width, size.height))
		}
	}
	
	object BubbleChartComp extends Component {
		
		preferredSize = SIZE
		
		val plot = ???
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			// FILL ME IN
		}
	}
	
	def top = new MainFrame {
		title = "Hello Visdome!"
		contents = new TabbedPane {
			pages += new TabbedPane.Page("Overlay", OverlayComp)
			pages += new TabbedPane.Page("Rect", RectPlotComp)
			pages += new TabbedPane.Page("Line", LinePlotComp)
			pages += new TabbedPane.Page("Scatter", ScatterPlotComp)
			pages += new TabbedPane.Page("FacetWrap", FacetWrapComp)
			pages += new TabbedPane.Page("Histogram", HistogramComp)
			pages += new TabbedPane.Page("Log10", Log10ScatterPlotComp)
			pages += new TabbedPane.Page("Histogram2", Histogram2Comp)
			pages += new TabbedPane.Page("Heatmap", HeatMapComp)
			pages += new TabbedPane.Page("BubbleChart", BubbleChartComp)
		}
	}
}