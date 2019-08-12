package org.w3.banana

trait RDFQuadDSL[Rdf <: RDF] extends RDFDSL[Rdf] { this: RDFQuadOps[Rdf] =>

  // quad

  object Quad {
    def apply(s: Rdf#Node, p: Rdf#URI, o: Rdf#Node): Rdf#Quad = makeQuad(s, p, o)
    def apply(s: Rdf#Node, p: Rdf#URI, o: Rdf#Node, c: Rdf#Node): Rdf#Quad = makeQuad(s, p, o, c)
    def unapply(quad: Rdf#Quad): Some[(Rdf#Node, Rdf#URI, Rdf#Node, Rdf#Node)] =
      Some(fromQuad(quad))
  }


  // Quad Graph

  object QuadGraph {
    def empty: Rdf#QuadGraph = emptyQuadGraph
    def apply(elems: Rdf#Quad*): Rdf#QuadGraph = makeQuadGraph(elems.toIterable)
    def apply(it: Iterable[Rdf#Quad]): Rdf#QuadGraph = makeQuadGraph(it)
  }

}
