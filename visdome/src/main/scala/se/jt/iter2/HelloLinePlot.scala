package se.jt.iter2

import swing._

object HelloLinePlot extends SimpleSwingApplication {
	
		
import Scale._
		
	case class Data(t:Double, position:Double, group:String)
		
	val ts = (0 until 100).map(_ / 10.0)
	val y1 = ts.map(t => Data(t, math.sin(t), "y1"))
	val y2 = ts.map(t => Data(t, math.sin(2 * t), "y2"))
	val y3 = ts.map(t => Data(t, math.sin(1+t), "y3"))
	
	val data = y1 ++ y2 ++ y3
	
	object HelloLinePlotPlot extends Component {
		
		preferredSize = new java.awt.Dimension(300, 300)
		
		val plot = new Plot(data).x(_.t).y(_.position).color(_.group).geom(new LinePlot)
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			plot.render(g, Geom.Rect(0, 0, size.width, size.height))
		}
	}
	
	object HelloScatterPlotPlot extends Component {
		
		preferredSize = new java.awt.Dimension(300, 300)
		
		val plot = new Plot(data).x(_.group).y(_.position).color(_.group).geom(new ScatterPlot)
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			plot.render(g, Geom.Rect(0, 0, size.width, size.height))
		}
	}
	
	object HelloHistogram extends Component {
		
		preferredSize = new java.awt.Dimension(300, 300)
		
		val plot = new Plot(data).x(_.position).geom(new Histogram)
		
		override def paintComponent(g: Graphics2D) = {
			super.paintComponent(g)
			plot.render(g, Geom.Rect(0, 0, size.width, size.height))
		}
	}
	
	def top = new MainFrame {
		title = "Hello line plot!"
		contents = new TabbedPane {
			pages += new TabbedPane.Page("Histogram", HelloHistogram)
			pages += new TabbedPane.Page("LinePlot", HelloLinePlotPlot)
			pages += new TabbedPane.Page("Scatter", HelloScatterPlotPlot)
		}
	}
}