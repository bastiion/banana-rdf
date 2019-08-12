package org.w3.banana.syntax

import org.w3.banana._

trait QuadSyntax[Rdf <: RDF] { self: RDFSyntax[Rdf] =>

  implicit def quadSyntax(quad: Rdf#Quad) = new QuadW[Rdf](quad)

}

class QuadW[Rdf <: RDF](val quad: Rdf#Quad) extends AnyVal {

  def subject(implicit ops: RDFQuadOps[Rdf]): Rdf#Node = {
    val (s, _, _,_) = ops.fromQuad(quad)
    s
  }

  def predicate(implicit ops: RDFQuadOps[Rdf]): Rdf#URI = {
    val (_, p, _, _) = ops.fromQuad(quad)
    p
  }

  def objectt(implicit ops: RDFQuadOps[Rdf]): Rdf#Node = {
    val (_, _, o, _) = ops.fromQuad(quad)
    o
  }

  def graphname(implicit ops: RDFQuadOps[Rdf]): Rdf#Node = {
    val (_, _, _, c) = ops.fromQuad(quad)
    c
  }


  def resolveAgainst(baseUri: Rdf#URI)(implicit ops: RDFQuadOps[Rdf]): Rdf#Quad = {
    import ops._
    val (s, p, o, c) = ops.fromQuad(quad)
    ops.makeQuad(s.resolveAgainst(baseUri), p, o.resolveAgainst(baseUri), c)
  }

  def relativize(baseUri: Rdf#URI)(implicit ops: RDFQuadOps[Rdf]): Rdf#Quad = {
    import ops._
    val (s, p, o, c) = ops.fromQuad(quad)
    ops.makeQuad(s.relativize(baseUri), p, o.relativize(baseUri), c)
  }

  def relativizeAgainst(baseUri: Rdf#URI)(implicit ops: RDFQuadOps[Rdf]): Rdf#Quad = {
    import ops._
    val (s, p, o, c) = ops.fromQuad(quad)
    ops.makeQuad(s.relativizeAgainst(baseUri), p, o.relativizeAgainst(baseUri), c)
  }

}
