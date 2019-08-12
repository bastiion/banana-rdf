package org.w3.banana.syntax

import org.w3.banana._

trait QuadMatchSyntax[Rdf <: RDF] { self: RDFSyntax[Rdf] =>

  implicit def quadMatchW(quadMatch: QuadMatch[Rdf]) =
    new QuadMatchW[Rdf](quadMatch)

}

class QuadMatchW[Rdf <: RDF](val quadMatch: QuadMatch[Rdf]) extends AnyVal {

  import quadMatch.{ _1 => sMatch, _2 => pMatch, _3 => oMatch, _4 => cMatch }

  def resolveAgainst(baseUri: Rdf#URI)(implicit ops: RDFOps[Rdf]): QuadMatch[Rdf] = {
    import ops._
    val s = sMatch.resolveAgainst(baseUri)
    val o = oMatch.resolveAgainst(baseUri)
    (s, pMatch, o, cMatch)
  }

  def relativize(baseUri: Rdf#URI)(implicit ops: RDFOps[Rdf]): QuadMatch[Rdf] = {
    import ops._
    val s = sMatch.relativize(baseUri)
    val o = oMatch.relativize(baseUri)
    (s, pMatch, o, cMatch)
  }

}
