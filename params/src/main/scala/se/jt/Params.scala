/*
 * Params
 *	Copyright (C) 2014 Johan Teleman
 */
package se.jt

object Params {
	
	case class ParamUpdater(
			name:String, 
			desc:String, 
			curr:Any, 
			update:String => Unit
		)
	
	class Param[T](val default:T, val desc:String) {
		var value = default
	}
	
	case class Plong(l:Long, 		override val desc:String) extends Param[Long](l, desc)
	case class Pint(i:Int, 			override val desc:String) extends Param[Int](i, desc)
	case class Pouble(d:Double, 	override val desc:String) extends Param[Double](d, desc)
	case class Pring(s:String, 		override val desc:String) extends Param[String](s, desc)
	case class Poolean(b:Boolean, 	override val desc:String) extends Param[Boolean](b, desc)
	
	
	implicit class PlongWrapper(l:Long) 		{ def ##(desc:String) = Plong(l, desc) }
	implicit class PintWrapper(i:Int) 			{ def ##(desc:String) = Pint(i, desc) }
	implicit class PoubleWrapper(d:Double) 		{ def ##(desc:String) = Pouble(d, desc) }
	implicit class PringWrapper(s:String) 		{ def ##(desc:String) = Pring(s, desc) }
	implicit class PooleanWrapper(b:Boolean) 	{ def ##(desc:String) = Poolean(b, desc) }
	
	
	implicit def pbool2bool(p:Poolean) 		= p.value
	implicit def pint2int(p:Pint) 			= p.value
	implicit def plong2long(p:Plong) 		= p.value
	implicit def pdouble2double(p:Pouble) 	= p.value
	implicit def pstring2string(p:Pring) 	= p.value
}



trait Params {
	
	import Params._
	
	private def isParam(c:java.lang.Class[_]):Boolean =
		if 		(c == classOf[AnyRef]) 		false
		else if (c == classOf[Param[_]]) 	true
		else 								isParam(c.getSuperclass)
	
	def getUpdaters:Map[String, ParamUpdater] = {
		val c = this.getClass()
		val paramFields = c.getDeclaredFields().filter(f => 
				!f.getType.isPrimitive() && isParam(f.getType)
			).map(_.getName)
		paramFields map (pf => {
			
			val m = c.getMethod(pf)
			def fix[T](p:Param[T], up:String => Unit) = 
				ParamUpdater(pf, p.desc, p.value, up)
					
			pf -> (m.invoke(this) match {
				case p:Pring => 	fix(p, str => p.value = str)
				case p:Pint => 		fix(p, str => p.value = str.toInt)
				case p:Poolean => 	fix(p, str => p.value = str.toBoolean)
				case p:Pouble => 	fix(p, str => p.value = str.toDouble)
				case p:Plong => 	fix(p, str => p.value = str.toLong)
				case _ => throw new Exception("method returning Param didn't return param... confused!")
			})
		}) toMap
	}
}
