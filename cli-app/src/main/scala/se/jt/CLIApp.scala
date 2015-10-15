package se.jt

import grizzled.slf4j.Logging
import collection.mutable.Queue
import java.io.File
import scala.io.Source

trait CLIApp extends Logging {

	private var applicationName	= ""
	private var args = Array[String]()
	
	import Params._
	
	var quiet = false
	
		
	def stripParam(p:String) = stripKeyVal(p.drop(2))
	
	def stripKeyVal(kv:String) = {
		val t = kv.split("=", 2).map(_.trim)
		(t(0), if (t.length == 2) Some(t(1)) else None)
	}
	
	def parseArgs(
			name:String, 
			version:String,
			args:Array[String], 
			params:Params, 
			reqOrder:List[String], 
			rest:Option[String]
	):Seq[String] = {
		
		val opts = params.opts
		
		val errs = new Queue[String]()
		def setError(msg:String) = {
			errs += msg
		}
		
		def parse(args:List[String], reqs:List[String]):Unit = {
			args match {
				case arg::as =>
					if (arg.startsWith("--")) {
						val (param, value) = stripParam(arg)
						opts.get(param) match {
							case Some(pu) =>
								value match {
									case Some(v) =>
										pu.update(v)
									case None =>
										pu.update("true")
								}
							case None =>
								setError("Error parsing '%s'. Option does not exist.".format(param))
						}
						parse(as, reqs)
					} else {
						reqs match {
							case req::rs =>
								opts(req).update(arg)
								parse(as, rs)
							case Nil =>
								if (rest.isDefined) 
									opts(rest.get).update(args.mkString(" "))
								else
									setError("Too many arguments provided!")
						}
					}
				case Nil =>
					if (reqs.nonEmpty)
						setError("Not enough arguments!")
					rest.foreach(x =>
						if (opts(x).curr.isEmpty)
							setError("Not enough arguments!")
					)
			}
		}
		parse(args.toList, reqOrder)
		
		if (errs.nonEmpty && !quiet)
			print(usage(name, version, args, params, reqOrder, rest))
		errs
	}
	
	
	
	def parseParams(
			params:Params, 
			path:String
	):Seq[String] = {
		val f = new File(path)
		val opts = params.opts
		val errs = new Queue[String]()
		def setError(msg:String) = {
			errs += msg
		}
		
		if (f.exists) {
			for (line <- Source.fromFile(f).getLines) {
				val (param, value) = stripKeyVal(line)
				opts.get(param) match {
					case Some(pu) =>
						value match {
							case Some(v) =>
								pu.update(v)
							case None =>
								pu.update("true")
						}
					case None =>
						setError("Error parsing '%s'. Option does not exist.".format(param))
				}
			}
		}
		errs
	}
	
	
	def usage(
			name:String,
			version:String,
			args:Array[String], 
			params:Params, 
			reqOrder:List[String], 
			rest:Option[String]
	) = {
		def currToString(c:Any) =
			c match {
				case Some(a) => a.toString
				case None => "-"
				case x => x.toString
			}
		val template = 
			"usage:\n> java -jar "+name+"-"+version+".jar [OPTIONS]" +
				reqOrder.map(" "+_).mkString + 
				rest.map(" " + _ + "+").getOrElse("")
		val pus = params.opts.values.toSeq
		val maxNameLength = pus.map(_.name.length).max
		val maxDefLength = math.max(16, pus.map(pu => currToString(pu.curr).length).max)
		val fString = "    %"+maxNameLength+"s %s\t%s"
		val header = fString.format("PARAMETER", "DEFAULT".padTo(maxDefLength, " ").mkString, "DESCRIPTION")
		val opts = pus.sortBy(_.name).map(pu => 
						fString.format(
								pu.name, 
								currToString(pu.curr).padTo(maxDefLength, " ").mkString, 
								pu.desc
						)).mkString("\n")
		List(template, "OPTIONS:", header, opts).mkString("\n")
	}
	
	
	def niceTiming(t:Long):String = {
		
		val ms = t % 1000
		var x = t / 1000
		val s = x % 60
		x = x / 60
		val m = x % 60
		x = x / 60
		val h = x % 24
		val d = x / 24
		val str = "%d days %02d:%02d:%02d.%03d".format(d, h, m, s, ms)
		val init = str.takeWhile(c => !c.isDigit || c == '0')
		init.replace('0', '_') + str.drop(init.length)
	}
	
	
	def failOnError(errs:Seq[String]) = 
		if (errs.nonEmpty) {
			println
			println
			for (err <- errs)
				println(err)
			System.exit(1)		
		}
	
}