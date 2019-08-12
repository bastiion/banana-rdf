package org.w3.banana

import org.scalatest.{Matchers, WordSpec}

class RDFQuadOpsTest[Rdf <: RDF]()(implicit ops: RDFQuadOps[Rdf]) extends WordSpec with Matchers  {

  import ops._

  val foaf = FOAFPrefix[Rdf]
  val rdfs = RDFSPrefix[Rdf]
  val context = URI(foaf.prefixIri)
  val contextFun = URI(foaf.prefixIri + "fun/")

  "can build a quad graph and get its named graph, default graph and union" in {


    Triple(foaf.Agent, rdf.typ, rdfs.Class)
    val g = makeQuadGraph(List(
      Quad(foaf.Agent, rdf.typ, rdfs.Class, context ) ,
      Quad(foaf.Agent, rdfs.label, Literal("Agent"), context ) ,
      Quad(foaf.Agent, rdfs.comment, Literal("not 007"), contextFun ),
      Quad(foaf.Agent, rdfs.isDefinedBy, context)
    ))

    val ng = getNamedGraph(g, context)
    val defg = getDefaultGraph(g)
    val uniong = asTripleGraph(g) union defg

    println(uniong.toString)

    ng.isIsomorphicWith((foaf.Agent.a(rdfs.Class) -- rdfs.label ->- "Agent").graph) shouldBe true

    defg.isIsomorphicWith((foaf.Agent -- rdfs.isDefinedBy ->- context).graph) shouldBe true

    uniong.isIsomorphicWith(
      (foaf.Agent.a(rdfs.Class)
      -- rdfs.label ->- "Agent"
      -- rdfs.comment ->- "not 007"
      -- rdfs.isDefinedBy ->- context
      ).graph) shouldBe true

  }

  "can convert quad to triple" in {

    Quad(foaf.Agent, rdf.typ, rdfs.Class, context ).asTriple shouldEqual Triple(foaf.Agent, rdf.typ, rdfs.Class)


  }


}