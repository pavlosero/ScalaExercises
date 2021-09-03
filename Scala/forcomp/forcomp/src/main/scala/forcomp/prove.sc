val ocs = List(('a', 1), ('b', 2), ('c', 1))
val y = List(('c', 1))

ocs.toMap.foldLeft(Map[Char, Int]())((acc, occ) => {
  acc.updated(occ._1, occ._2 - y.toMap.withDefaultValue(0)(occ._1))
}).filter(p => p._2>0).toList
