package se.jt

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TicksTest extends FunSuite {

	import Scale._
	
	test("linTicks") {
		val t = linTicks(0.8, 3.2)
		assert(t.contains(1.0))
		assert(t.contains(2.0))
		assert(t.contains(3.0))
		
		val t2 = linTicks(80, 320)
		assert(t2.contains(100.0))
		assert(t2.contains(200.0))
		assert(t2.contains(300.0))
	}
}