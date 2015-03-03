/*
 * Params
 *	Copyright (C) 2014 Johan Teleman
 */
package se.jt

object Params {
	
	class ParamException(msg:String) extends Exception(msg) {}
	
	case class ParamUpdater(
			name:String, 
			desc:String, 
			curr:Any, 
			update:String => Unit,
			innerType:String
		)
	
	class Param[T](val default:Option[T], val desc:String) {
		var _value = default
		def value = 
			_value.getOrElse { throw new ParamException("unintialized param") }
		def value_=(t:T) = _value = Some(t)
	}
	
	case class Plong(l:Long, 		override val desc:String) extends Param[Long](Some(l), desc)
	case class Pint(i:Int, 			override val desc:String) extends Param[Int](Some(i), desc)
	case class Pouble(d:Double, 	override val desc:String) extends Param[Double](Some(d), desc)
	case class Pring(s:String, 		override val desc:String) extends Param[String](Some(s), desc)
	case class Poolean(b:Boolean, 	override val desc:String) extends Param[Boolean](Some(b), desc)
	case class Plist(l:List[String], override val desc:String, sep:Char = ' ') extends Param[List[String]](Some(l), desc)
	
	case class ReqBoolean(	override val desc:String) extends Param[Boolean](None, desc)
	case class ReqInt(		override val desc:String) extends Param[Int](None, desc)
	case class ReqLong(		override val desc:String) extends Param[Long](None, desc)
	case class ReqDouble(	override val desc:String) extends Param[Double](None, desc)
	case class ReqString(	override val desc:String) extends Param[String](None, desc)
	case class ReqList(		override val desc:String, sep:Char = ' ') extends Param[List[String]](None, desc)
	/*
	case class PoptionLong(override val desc:String) extends Param[Option[Long]](None, desc)
	case class PoptionInt(override val desc:String) extends Param[Option[Int]](None, desc)
	case class PoptionDouble(override val desc:String) extends Param[Option[Double]](None, desc)
	case class PoptionString(override val desc:String) extends Param[Option[String]](None, desc)
	case class PoptionBoolean(override val desc:String) extends Param[Option[Long]](None, desc)
	*/
	
	implicit class PlongWrapper(l:Long) 		{ def ##(desc:String) = Plong(l, desc) }
	implicit class PintWrapper(i:Int) 			{ def ##(desc:String) = Pint(i, desc) }
	implicit class PoubleWrapper(d:Double) 		{ def ##(desc:String) = Pouble(d, desc) }
	implicit class PringWrapper(s:String) 		{ def ##(desc:String) = Pring(s, desc) }
	implicit class PooleanWrapper(b:Boolean) 	{ def ##(desc:String) = Poolean(b, desc) }
	implicit class PlistWrapper(l:Seq[String]) 	{ def ##(desc:String) = Plist(l.toList, desc) }
	
	
	implicit def pbool2bool(p:Param[Boolean]) 		= p.value
	implicit def pint2int(p:Param[Int]) 			= p.value
	implicit def plong2long(p:Param[Long]) 			= p.value
	implicit def pdouble2double(p:Param[Double]) 	= p.value
	implicit def pstring2string(p:Param[String]) 	= p.value
	implicit def plist2list(p:Param[List[String]]) 	= p.value
	/*
	implicit def poption2bool(p:Poption[Boolean]) = 
		p.value.getOrElse { throw new ParamException("unintialized param") }
	implicit def poption2int(p:Poption[Int]) = 
		p.value.getOrElse { throw new ParamException("unintialized param") }
	implicit def poption2long(p:Poption[Long]) = 
		p.value.getOrElse { throw new ParamException("unintialized param") }
	implicit def poption2double(p:Poption[Double]) = 
		p.value.getOrElse { throw new ParamException("unintialized param") }
	implicit def poption2string(p:Poption[String]) = 
		p.value.getOrElse { throw new ParamException("unintialized param") }

*/ 
}



trait Params {
	
	import Params._
	
	private def isParam(c:java.lang.Class[_]):Boolean =
		if 		(c == classOf[AnyRef]) 		false
		else if (c == classOf[Param[_]]) 	true
		else if (c.getSuperclass == null)	false
		else 								isParam(c.getSuperclass)
	
	def opts:Map[String, ParamUpdater] = {
		val c = this.getClass()
		val paramFields = c.getDeclaredFields().filter(f => 
				!f.getType.isPrimitive() && isParam(f.getType)
			).map(_.getName)
		paramFields map (pf => {
			
			val m = c.getMethod(pf)
			def fix[T](p:Param[T], up:String => Unit)(implicit m: reflect.Manifest[T]) = 
				ParamUpdater(pf, p.desc, p._value, up, m.toString)
					
			pf -> (m.invoke(this) match {
				case p:Poolean => 	fix(p, str => p.value = str.toBoolean)
				case p:Pint => 		fix(p, str => p.value = str.toInt)
				case p:Plong => 	fix(p, str => p.value = str.toLong)
				case p:Pouble => 	fix(p, str => p.value = str.toDouble)
				case p:Pring => 	fix(p, str => p.value = str)
				case p:Plist => 	fix(p, str => p.value = str.split(p.sep).toList)
				case p:ReqBoolean => 	fix(p, str => p.value = str.toBoolean)
				case p:ReqInt => 		fix(p, str => p.value = str.toInt)
				case p:ReqLong => 		fix(p, str => p.value = str.toLong)
				case p:ReqDouble => 	fix(p, str => p.value = str.toDouble)
				case p:ReqString => 	fix(p, str => p.value = str)
				case p:ReqList => 		fix(p, str => p.value = str.split(p.sep).toList)
				case _ => throw new Exception("method returning Param didn't return param... confused!")
			})
		}) toMap
	}
	
	def desc:String = {
		opts.values.map(pu => "%s\t%s".format(pu.name, pu.desc)).mkString("\n")
	}
}
