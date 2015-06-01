package se.jt

import scala.swing._
import scala.swing.event._

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

import java.awt.Color

object Visdome extends SimpleSwingApplication {
	
	import Scale._
	import Stratifier._
	case class Data(t:Double, position:Double, group:String)
	case class Point(x:Int, y:Double)
	
	val ts = (0 until 100).map(_ / 10.0)
	val y1 = ts.map(t => Data(t, math.sin(t), "y1"))
	val y2 = ts.map(t => Data(t, math.sin(2 * t), "y2"))
	val y3 = ts.map(t => Data(t, math.sin(1+t), "y3"))
	
	val data = y1 ++ y2 ++ y3
	
	val points = (0 until 10000).map(i => Point(Random.nextInt(5), Random.nextDouble))
	
	val SIZE = new java.awt.Dimension(1000, 1000)
	
	case class Marker[D, X, Y](ctrl:PlotControl[D, X, Y], px:Int, py:Int)
	
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
	
	def top = new MainFrame {
		title = "Hello Visdome!"
		contents = new TabbedPane {
			pages += new TabbedPane.Page("Line", LinePlotComp)
			pages += new TabbedPane.Page("Scatter", ScatterPlotComp)
			pages += new TabbedPane.Page("FacetWrap", FacetWrapComp)
			pages += new TabbedPane.Page("Histogram", HistogramComp)
			pages += new TabbedPane.Page("Log10", Log10ScatterPlotComp)
			pages += new TabbedPane.Page("Histogram2", Histogram2Comp)
			pages += new TabbedPane.Page("Heatmap", HeatMapComp)
		}
	}
}