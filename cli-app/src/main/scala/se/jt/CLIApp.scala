package se.jt

import grizzled.slf4j.Logging
import collection.mutable.Queue

trait CLIApp extends Logging {

	private var applicationName	= ""
	private var args = Array[String]()
	
	import Params._
	
	
	def parseArgs(
			applicationName:String, 
			args:Array[String], 
			params:Params, 
			reqOrder:List[String], 
			rest:Option[String]
	):Seq[String] = {
		/*args.find(a => a.startsWith("--log-dir=")) match {
			case Some(a) => logDir = a.dropWhile(_ != '=').tail.toString
			case None => {}
		}*/
		
		this.applicationName = applicationName
		this.args = args
		
		val opts = params.opts
		
		def stripParam(p:String) = {
			val t = p.drop(2).split("=", 2)
			(t(0), if (t.length == 2) Some(t(1)) else None)
		}
		
		val errs = new Queue[String]()
		def setError(msg:String) = errs += msg
		
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
			}
		}
		parse(args.toList, reqOrder)
		
		errs
	}
	
}