package org.w3.banana.plantain.model

import xyz.hyperreal.btree.MemoryBPlusTree

import scala.collection.mutable

trait BTreeSPODictionary[K, S, P, O] extends SPODictionary[K, S, P, O] {

  def subjectTree: MemoryBPlusTree[K, S]

  def predicateTree: MemoryBPlusTree[K, P]

  def objectTree: MemoryBPlusTree[K, O]

  def rsubjectMap: mutable.Map[S, K]

  def rpredicateMap: mutable.Map[P, K]

  def robjectMap: mutable.Map[O, K]

  override def subjectOf(s: K): S = subjectTree.search(s).get

  override def objectOf(o: K): O = objectTree.search(o).get

  override def predicateOf(p: K): P = predicateTree.search(p).get

  override def removeSubjectKey(s: K): SPODictionary[K, S, P, O] = {
    subjectTree.delete(s)
    this
  }

  override def removeObjectKey(o: K): SPODictionary[K, S, P, O] = {
    objectTree.delete(o)
    this
  }

  override def removePredicateKey(p: K): SPODictionary[K, S, P, O] = {
    predicateTree.delete(p)
    this
  }

  override def fromSubject(s: S): K = rsubjectMap(s)

  override def fromObject(o: O): K = robjectMap(o)

  override def fromPredicate(p: P): K = rpredicateMap(p)

  override def hasSubject(s: S): Boolean = rsubjectMap.contains(s)

  override def hasObject(o: O): Boolean = robjectMap.contains(o)

  override def hasPredicate(p: P): Boolean = rpredicateMap.contains(p)

}

