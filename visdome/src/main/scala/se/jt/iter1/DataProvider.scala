package se.jt.iter1

object DataProvider {
	def apply(col:(String, Iterable[Any])*):DataProvider = 
		new MappedDataProvider(col.toMap)
}

trait DataProvider {

	def cols:Iterable[String]
	def apply(col:String):Iterable[Any]
	def multiply(name:String, groupName:String, col:(String, Iterable[Any])*):DataProvider
}

class MappedDataProvider(
		val m:Map[String, Iterable[Any]]
) extends DataProvider {
	def cols = m.keys
	def apply(col:String) = m(col)
	def repeat(s:Iterable[Any], i:Int):Iterable[Any] =
		if (i <= 0) Nil
		else if (i == 1) s
		else s ++ repeat(s, i-1)
	def multiply(
			name:String, 
			groupName:String, 
			col:(String, Iterable[Any])*
	):DataProvider = {
		val m2 = m.map(t => t._1 -> repeat(t._2, col.length)) ++
				Array(name -> col.flatMap(_._2), 
					groupName -> col.flatMap(t => List[String]().padTo(t._2.size, t._1))
				)
		new MappedDataProvider(m2)
	}
}