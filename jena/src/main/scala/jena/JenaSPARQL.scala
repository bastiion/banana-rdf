package org.w3.banana.jena

import org.w3.banana._

import com.hp.hpl.jena.query.{ Query => JenaQuery, QuerySolution, ResultSet }

trait JenaSPARQL extends SPARQL {

  type Query = JenaQuery

  type SelectQuery = JenaQuery

  type ConstructQuery = JenaQuery

  type AskQuery = JenaQuery

  type Solution = QuerySolution

  type Solutions = ResultSet

}