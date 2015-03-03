package se.jt

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParamsTest extends FunSuite {

	import Params._
	
	class P1 extends Params {
			val s = "hi"
			val flag = true
			val num = 100
			val wDesc = Pint(666, "supposedly evil default number")
		}
	
	class P2 extends Params {
		val s = 	"hi" 	## "this is a string param"
		val flag = 	true 	## ""
		val num = 	100 	## ""
	}
	
	class P3 extends Params {
		val s 		= "hi" 	## "this is a string param"
		val flag 	= ReqBoolean("needs to be set!")
	}
	
	test("basic") {
		val p = new P1 
		val opts = p.opts
		assert(1 === opts.size)
		assert(opts contains "wDesc")
	}
	
	test("implicits") {
		val p = new P2
		
		val s:String = p.s
		val flag:Boolean = p.flag
		val num:Int = p.num
	}
	
	test("update") {
		val p = new P2
		
		val opts = p.opts
		try {
			opts("s") match {
				case ParamUpdater(n, d, x, up, innerType) =>
					up("hello!")
					assert("hello!" === p.s.value)
			}
			
			opts("flag") match {
				case ParamUpdater(n, d, x, up, innerType) =>
					up("false")
					assert(!p.flag.value)
			}
			
			opts("num") match {
				case ParamUpdater(n, d, x, up, innerType) =>
					up("42")
					assert(42 === p.num.value)
			}
		} catch {
			case _:Throwable =>
				fail("Threw for valid operations. 's', 'flag',and 'num' should all be valid opts.")
		}
	}
	
	test("update2") {
		val p = new P2
		
		val opts = p.opts
		opts("s").update("hello!")
		assert("hello!" === p.s.value)
		
		opts("flag").update("false")
		assert(!p.flag)
		
		opts("num").update("42")
		assert(42 === p.num.value)
	}
	
	test("desc") {
		val p = new P2
		
		assert(p.desc === "s\tthis is a string param\nflag\t\nnum\t")
	}
	
	test("poption") {
		val p = new P3
		
		val opts = p.opts
		assert(2 === opts.size)
		try {
			val b:Boolean = p.flag
			fail("did not throw exception when accessing uninitialized value")
		} catch {
			case pe:ParamException => {}
			case _:Throwable => 
				fail("threw wrong kind of exception when accessing uninitialized value")
				
		}
		
		opts("flag").update("true")
		assert(p.flag)
	}
}