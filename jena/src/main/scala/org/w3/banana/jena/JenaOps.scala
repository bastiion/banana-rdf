package org.w3.banana.jena

import org.apache.jena.datatypes.{BaseDatatype, RDFDatatype, TypeMapper}
import org.apache.jena.graph.{ Graph => JenaGraph, Node => JenaNode, Triple => JenaTriple, _}
import org.apache.jena.rdf.model.{Literal => JenaLiteral, Seq => _}
import org.apache.jena.sparql.core.{DatasetDescription, DatasetGraph, Quad => JenaQuad}
import org.apache.jena.sparql.util.DatasetUtils
import org.w3.banana._

import scala.collection.JavaConverters._

class JenaOps extends RDFQuadOps[Jena] with JenaMGraphOps with DefaultURIOps[Jena] {

  // graph

  val emptyGraph: Jena#Graph = Factory.createDefaultGraph

  def makeGraph(triples: Iterable[Jena#Triple]): Jena#Graph = {
    val graph: JenaGraph = Factory.createDefaultGraph
    triples.foreach { triple =>
      graph.add(triple)
    }
    graph
  }

  def getTriples(graph: Jena#Graph): Iterable[Jena#Triple] =
    graph.find(JenaNode.ANY, JenaNode.ANY, JenaNode.ANY).asScala.to[Iterable]

  // triple

  def makeTriple(s: Jena#Node, p: Jena#URI, o: Jena#Node): Jena#Triple = {
    JenaTriple.create(s, p, o)
  }

  def fromTriple(t: Jena#Triple): (Jena#Node, Jena#URI, Jena#Node) = {
    val s = t.getSubject
    val p = t.getPredicate
    val o = t.getObject
    if (p.isInstanceOf[Jena#URI])
      (s, p.asInstanceOf[Jena#URI], o)
    else
      throw new RuntimeException("fromTriple: predicate " + p.toString + " must be a URI")
  }

  // node

  def foldNode[T](node: Jena#Node)(funURI: Jena#URI => T, funBNode: Jena#BNode => T, funLiteral: Jena#Literal => T): T = node match {
    case iri: Jena#URI => funURI(iri)
    case bnode: Jena#BNode => funBNode(bnode)
    case literal: Jena#Literal => funLiteral(literal)
  }

  // URI

  def makeUri(iriStr: String): Jena#URI = { NodeFactory.createURI(iriStr).asInstanceOf[Node_URI] }

  def fromUri(node: Jena#URI): String =
    if (node.isURI)
      node.getURI
    else
      throw new RuntimeException("fromUri: " + node.toString() + " must be a URI")

  // bnode

  def makeBNode(): Node_Blank = NodeFactory.createBlankNode().asInstanceOf[Node_Blank]

  def makeBNodeLabel(label: String): Jena#BNode = {
    val id = BlankNodeId.create(label)
    NodeFactory.createBlankNode(id).asInstanceOf[Node_Blank]
  }

  def fromBNode(bn: Jena#BNode): String =
    if (bn.isBlank)
      bn.getBlankNodeId.getLabelString
    else
      throw new RuntimeException("fromBNode: " + bn.toString + " must be a BNode")

  // literal

  // TODO the javadoc doesn't say if this is thread safe
  lazy val mapper = TypeMapper.getInstance

  def jenaDatatype(datatype: Jena#URI) = {
    val iriString = fromUri(datatype)
    val typ = mapper.getTypeByName(iriString)
    if (typ == null) {
      val datatype = new BaseDatatype(iriString)
      mapper.registerDatatype(datatype)
      datatype
    } else {
      typ
    }
  }

  val __xsdString: RDFDatatype = mapper.getTypeByName("http://www.w3.org/2001/XMLSchema#string")
  val __xsdStringURI: Jena#URI = makeUri("http://www.w3.org/2001/XMLSchema#string")
  val __rdfLangStringURI: Jena#URI = makeUri("http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")

  def makeLiteral(lexicalForm: String, datatype: Jena#URI): Jena#Literal =
    if (datatype == __xsdStringURI)
      NodeFactory.createLiteral(lexicalForm, null, null).asInstanceOf[Node_Literal]
    else
      NodeFactory.createLiteral(lexicalForm, null, jenaDatatype(datatype)).asInstanceOf[Node_Literal]

  def makeLangTaggedLiteral(lexicalForm: String, lang: Jena#Lang): Jena#Literal =
    NodeFactory.createLiteral(lexicalForm, fromLang(lang), null).asInstanceOf[Node_Literal]

  def fromLiteral(literal: Jena#Literal): (String, Jena#URI, Option[Jena#Lang]) = {
    val lexicalForm = literal.getLiteralLexicalForm.toString
    val literalLanguage = literal.getLiteralLanguage
    def getDatatype: Jena#URI = {
      val typ = literal.getLiteralDatatype
      if (typ == null) __xsdStringURI else makeUri(typ.getURI)
    }
    if (literalLanguage == null || literalLanguage.isEmpty)
      (lexicalForm, getDatatype, None)
    else
      (lexicalForm, __rdfLangStringURI, Some(makeLang(literalLanguage)))
  }

  // lang

  def makeLang(langString: String) = langString

  def fromLang(lang: Jena#Lang) = lang

  // graph traversal

  val ANY: Jena#NodeAny = JenaNode.ANY.asInstanceOf[Node_ANY]

  implicit def toConcreteNodeMatch(node: Jena#Node): Jena#NodeMatch = node.asInstanceOf[Jena#Node]

  def foldNodeMatch[T](nodeMatch: Jena#NodeMatch)(funANY: => T, funConcrete: Jena#Node => T): T =
    if (nodeMatch == ANY)
      funANY
    else
      funConcrete(nodeMatch.asInstanceOf[JenaNode])

  def find(graph: Jena#Graph, subject: Jena#NodeMatch, predicate: Jena#NodeMatch, objectt: Jena#NodeMatch): Iterator[Jena#Triple] = {
    graph.find(subject, predicate, objectt).asScala.toIterator
  }

  // graph union

  def union(graphs: Seq[Jena#Graph]): Jena#Graph = {
    val g = Factory.createDefaultGraph
    graphs.foreach { graph =>
      val it = graph.find(JenaNode.ANY, JenaNode.ANY, JenaNode.ANY)
      while (it.hasNext) { g.add(it.next()) }
    }
    g
  }

  def diff(g1: Jena#Graph, g2: Jena#Graph): Jena#Graph = {
    val g = Factory.createDefaultGraph
    GraphUtil.addInto(g, g1)
    GraphUtil.delete(g, g2.find(JenaNode.ANY, JenaNode.ANY, JenaNode.ANY))
    g
  }

  // graph isomorphism

  def isomorphism(left: Jena#Graph, right: Jena#Graph): Boolean =
    left isIsomorphicWith right

  def graphSize(g: Jena#Graph): Int = g.size()

  override def emptyQuadGraph: Jena#QuadGraph = DatasetUtils.createJena#QuadGraph(new DatasetDescription())

  override def makeQuadGraph(it: Iterable[Jena#Quad]): Jena#QuadGraph = {
    val g = emptyQuadGraph
    it.foreach(g.add)
    g
  }

  override def getQuads(graph: Jena#QuadGraph): Iterable[Jena#Quad] = graph.find().asScala.toIterable

  override def makeQuad(s: Jena#Node, p: Jena#URI, o: Jena#Node, c: Jena#Node): Jena#Quad = JenaQuad.create(c, s, p ,o)

  override def makeQuad(s: Jena#Node, p: Jena#URI, o: Jena#Node): Jena#Quad =  JenaQuad.create( JenaQuad.defaultGraphIRI , s, p, o)

  override def fromQuad(quad: Jena#Quad): (Jena#Node, Jena#URI, Jena#Node, Jena#Node) = {
    quad.getPredicate match {
      case uri: Node_URI => (quad.getSubject, uri, quad.getObject, quad.getGraph)
      case _ => throw new RuntimeException("fromTriple: predicate " + quad.getPredicate.toString + " must be a URI")
    }
  }

  override def find(graph: Jena#QuadGraph, subject: Jena#Node, predicate: Jena#Node, objectt: Jena#Node, context: Jena#Node): Iterable[Jena#Quad] =
    graph.find(subject, predicate, objectt, context).asScala.toIterable

  override def asTripleGraph(graph: Jena#QuadGraph): Jena#Graph = graph.getUnionGraph

  override def asTriple(quad: Jena#Quad): JenaTriple = quad.asTriple()

  override def asQuad(triple: Jena#Triple, c: Option[Jena#Node]): Jena#Quad = JenaQuad.create(c.getOrElse(JenaQuad.defaultGraphIRI), triple)

  override def getDefaultGraph(graph: Jena#QuadGraph): Jena#Graph = graph.getDefaultGraph

  override def makeDefaultGraph(graph: Jena#Graph): Jena#QuadGraph = {
    val qgraph = emptyQuadGraph
    qgraph.setDefaultGraph(graph)
    qgraph
  }

  override def getNamedGraph(graph: Jena#QuadGraph, c: Jena#Node): Jena#Graph = graph.getGraph(c)

  override def toQuadGraph(it: Iterable[JenaTriple], c: Option[Jena#Node]): Jena#QuadGraph = {
    val qgraph = emptyQuadGraph
    val graph = makeGraph(it)
    c match {
      case Some(context) => qgraph.addGraph(context, graph)
      case _ => qgraph.addGraph(JenaQuad.defaultGraphIRI, graph)
    }
    qgraph
  }

  override def isDefaultGraph(quad: Jena#Quad): Boolean = quad.isDefaultGraph

}
