package org.w3.banana

import org.scalatest.{Matchers, WordSpec}
import org.w3.banana.{PrefixBuilder, RDF, RDFOps}

class RDFQuadOpsTest[Rdf <: RDF]()(implicit ops: RDFQuadOps[Rdf]) extends WordSpec with Matchers  {

  import ops._

  "can build a quad graph and get a named graph" in {

    val foaf = FOAFPrefix[Rdf]
    val rdfs = RDFSPrefix[Rdf]
    val context = makeUri(foaf.prefixIri)
    val contextFun = makeUri(foaf.prefixIri + "fun/")

    val g = makeQuadGraph(List(
      makeQuad(foaf.Agent, rdf.typ, rdfs.Class, context ) ,
      makeQuad(foaf.Agent, rdfs.label, makeLiteral("Agent"), context ) ,
      makeQuad(foaf.Agent, rdfs.comment, makeLiteral("not 007"), contextFun ),
      makeQuad(foaf.Agent, rdfs.comment, makeLiteral("not 007"), contextFun )
    ))

    getNamedGraph(g, context).isIsomorphicWith((foaf.Agent.a(rdfs.Class) -- rdfs.label ->- "Agent").graph) shouldBe true
    getNamedGraph(g, context).isIsomorphicWith((foaf.Agent.a(rdfs.Class) -- rdfs.label ->- "Agent").graph) shouldBe true

  }


}