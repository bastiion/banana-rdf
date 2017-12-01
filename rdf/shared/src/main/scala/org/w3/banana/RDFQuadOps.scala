package org.w3.banana

trait RDFQuadOps[Rdf <: RDF]
extends RDFOps[Rdf] {


  def emptyQuadGraph: Rdf#QuadGraph

  def makeQuadGraph(it: Iterable[Rdf#Quad]): Rdf#QuadGraph

  def getQuads(graph: Rdf#QuadGraph): Iterable[Rdf#Quad]

  // quad

  def makeQuad(s: Rdf#Node, p: Rdf#URI, o: Rdf#Node, c: Rdf#Node): Rdf#Quad

  def makeQuad(s: Rdf#Node, p: Rdf#URI, o: Rdf#Node): Rdf#Quad

  def fromQuad(quad: Rdf#Quad): (Rdf#Node, Rdf#URI, Rdf#Node, Rdf#Node)

  def find(graph: Rdf#QuadGraph, subject: Rdf#NodeMatch, predicate: Rdf#NodeMatch, objectt: Rdf#NodeMatch, context: Rdf#NodeMatch): Iterable[Rdf#Quad]

  def asTripleGraph(graph: Rdf#QuadGraph): Rdf#Graph

  def asTriple(quad: Rdf#Quad): Rdf#Triple

  def getNamedGraph(graph: Rdf#QuadGraph, c: Rdf#Node): Rdf#Graph

  def getDefaultGraph(graph: Rdf#QuadGraph): Rdf#Graph

}