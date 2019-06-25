package org.w3.banana.plantain.model

import xyz.hyperreal.btree.MemoryBPlusTree

import scala.collection.mutable

case class IntBTreeSPODictionary[S, P, O](
                                           subjectTree: MemoryBPlusTree[Int, S],
                                           predicateTree: MemoryBPlusTree[Int, P],
                                           objectTree: MemoryBPlusTree[Int, O],
                                           rsubjectMap: mutable.Map[S, Int],
                                           rpredicateMap: mutable.Map[P, Int],
                                           robjectMap: mutable.Map[O, Int]
                                         ) extends BTreeSPODictionary[Int, S, P, O] {

  var counter: Int = 0

  override def newFromTriple(s: S, p: P, o: O): (Int, Int, Int, SPODictionary[Int, S, P, O]) = {
    val sKey = rsubjectMap.get(s) match {
      case Some(_s) => _s
      case _ =>
        counter += 1
        subjectTree.insert(counter, s)
        rsubjectMap += (s -> counter)
        counter
    }

    val pKey = rpredicateMap.get(p) match {
      case Some(_p) => _p
      case _ =>
        counter += 1
        predicateTree.insert(counter, p)
        rpredicateMap += (p -> counter)
        counter
    }

    val oKey = robjectMap.get(o) match {
      case Some(_o) => _o
      case _ =>
        counter += 1
        objectTree.insert(counter, o)
        robjectMap += (o -> counter)
        counter
    }

    (sKey, pKey, oKey, this)
  }

}

object IntBTreeSPODictionary {

  def empty[S, P, O](order: Int): IntBTreeSPODictionary[S, P, O] = IntBTreeSPODictionary(
    new MemoryBPlusTree[Int, S](order),
    new MemoryBPlusTree[Int, P](order),
    new MemoryBPlusTree[Int, O](order),
    mutable.Map.empty[S, Int],
    mutable.Map.empty[P, Int],
    mutable.Map.empty[O, Int]
  )

}
