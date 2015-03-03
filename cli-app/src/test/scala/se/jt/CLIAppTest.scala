package se.jt

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CLIAppTest extends FunSuite {

	import Params._
	
	class P2 extends Params {
		val s = 	"hi" 	## "this is a string param"
		val flag = 	false 	## ""
		val num = 	100 	## ""
		val list = List[String]() ## "a list param for the remainder"
	}
	
	class TestApp extends CLIApp {}
	
	test("basic") {
		val p = new P2
		val t = new TestApp
		t.parseArgs("testApp", Array("--s=bye","--num=50","--flag"), p, List(), None)
		val s:String = p.s
		assert(s === "bye")
		val f:Boolean = p.flag
		assert(f === true)
		val n:Int = p.num
		assert(n === 50)
	}
	
	test("reqs") {
		val p = new P2
		val t = new TestApp
		t.parseArgs("testApp", Array("--flag", "bye", "50"), p, List("s", "num"), None)
		val s:String = p.s
		assert(s === "bye")
		val f:Boolean = p.flag
		assert(f === true)
		val n:Int = p.num
		assert(n === 50)
	}
	
	test("rest") {
		val p = new P2
		val t = new TestApp
		t.parseArgs("testApp", Array("--flag", "bye", "file1", "file2", "file3"), p, List("s"), Some("list"))
		val s:String = p.s
		assert(s === "bye")
		val f:Boolean = p.flag
		assert(f === true)
		val l:List[String] = p.list
		assert(l(0) === "file1")
		assert(l(1) === "file2")
		assert(l(2) === "file3")
	}
	
	test("non-existing option") {
		val p = new P2
		val t = new TestApp
		val errs = t.parseArgs("testApp", Array("--SCPIAHS"), p, List(), None)
		assert(errs.length === 1)
		assert(errs(0) === "Error parsing 'SCPIAHS'. Option does not exist.")
	}
	
	test("too many args") {
		val p = new P2
		val t = new TestApp
		val errs = t.parseArgs("testApp", Array("--s=bye", "SCPIAHS"), p, List(), None)
		assert(errs.length === 1)
		assert(errs(0) === "Too many arguments provided!")
	}
	
	test("too few args") {
		val p = new P2
		val t = new TestApp
		val errs = t.parseArgs("testApp", Array("--s=bye"), p, List("num"), None)
		assert(errs.length === 1)
		assert(errs(0) === "Not enough arguments!")
	}
}