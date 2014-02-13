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
		val s = "hi" ## "this is a string param"
		val flag = true ## ""
		val num = 100 ## ""
	}
	
	test("basic") {
		val p = new P1 
		val opts = p.getUpdaters
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
		
		val opts = p.getUpdaters
		try {
			opts("s") match {
				case ParamUpdater(n, d, x, up) =>
					up("hello!")
					assert("hello!" === p.s.value)
			}
			
			opts("flag") match {
				case ParamUpdater(n, d, x, up) =>
					up("false")
					assert(!p.flag.value)
			}
			
			opts("num") match {
				case ParamUpdater(n, d, x, up) =>
					up("42")
					assert(42 === p.num.value)
			}
		} catch {
			case _:Throwable =>
				fail("Threw for valid operations. 's', 'flag',and 'num' should all be valid opts.")
		}
	}
}