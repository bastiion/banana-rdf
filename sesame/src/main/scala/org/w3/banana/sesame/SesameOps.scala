package org.w3.banana.sesame

import java.util.Locale

import org.openrdf.model._
import org.openrdf.model.impl._
import org.openrdf.model.util._
import org.w3.banana._

import scala.collection.JavaConverters._

class SesameOps extends RDFQuadOps[Sesame] with SesameMGraphOps with DefaultURIOps[Sesame] {

  val valueFactory: ValueFactory = ValueFactoryImpl.getInstance()

  // graph

  def emptyGraph: Sesame#Graph = new LinkedHashModel

  def makeGraph(it: Iterable[Sesame#Triple]): Sesame#Graph = {
    val graph = new LinkedHashModel
    it foreach { t => graph add t }
    graph
  }

  def getTriples(graph: Sesame#Graph): Iterable[Sesame#Triple] = graph.asScala

  // triple

  def makeTriple(s: Sesame#Node, p: Sesame#URI, o: Sesame#Node): Sesame#Triple =
    s match {
      case res:Resource=>  new StatementImpl(res, p, o)
      case _=> throw new RuntimeException("makeTriple: in Sesame subject " + p.toString + " must be a either URI or BlankNode")
    }

  def fromTriple(t: Sesame#Triple): (Sesame#Node, Sesame#URI, Sesame#Node) =  (t.getSubject, t.getPredicate, t.getObject)

  // node

  def foldNode[T](node: Sesame#Node)(funURI: Sesame#URI => T, funBNode: Sesame#BNode => T, funLiteral: Sesame#Literal => T): T = node match {
    case iri: Sesame#URI => funURI(iri)
    case bnode: Sesame#BNode => funBNode(bnode)
    case literal: Sesame#Literal => funLiteral(literal)
  }

  // URI

  /**
   * we provide our own builder for Sesame#URI to relax the constraint "the URI must be absolute"
   * this constraint becomes relevant only when you add the URI to a Sesame store
   */
  def makeUri(iriStr: String): Sesame#URI = {
    try {
      new URIImpl(iriStr)
    } catch {
      case iae: IllegalArgumentException =>
        new URI {
          override def equals(o: Any): Boolean = o.isInstanceOf[URI] && o.asInstanceOf[URI].toString == iriStr
          def getLocalName: String = iriStr
          def getNamespace: String = ""
          override def hashCode: Int = iriStr.hashCode
          override def toString: String = iriStr
          def stringValue: String = iriStr
        }
    }

  }

  def fromUri(node: Sesame#URI): String = node.toString

  // bnode

  def makeBNode() = valueFactory.createBNode()

  def makeBNodeLabel(label: String): Sesame#BNode = new BNodeImpl(label)

  def fromBNode(bn: Sesame#BNode): String = bn.getID

  // literal

  val __xsdString = makeUri("http://www.w3.org/2001/XMLSchema#string")
  val __rdfLangString = makeUri("http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")

  class LangLiteral(label: String, language: String) extends LiteralImpl(label, language) {
    this.setDatatype(__rdfLangString)
  }

  def makeLiteral(lexicalForm: String, datatype: Sesame#URI): Sesame#Literal =
    new LiteralImpl(lexicalForm, datatype)

  def makeLangTaggedLiteral(lexicalForm: String, lang: Sesame#Lang): Sesame#Literal =
    new LangLiteral(lexicalForm, lang)

  def fromLiteral(literal: Sesame#Literal): (String, Sesame#URI, Option[Sesame#Lang]) =
    (literal.getLabel, literal.getDatatype, Option(literal.getLanguage))

  /**
    *  language tags are cases insensitive according to
    * <a href="http://tools.ietf.org/html/bcp47#section-2.1.1">RFC 5646: Tags for Identifying Languages</a>
    * which is referenced by <a href="http://www.w3.org/TR/rdf11-concepts/#section-Graph-Literal">RDF11 Concepts</a>.
    * Sesame does not take this into account, so canonicalise here to lower case. ( The NTriples Tests don't pass
    * if the `.toLowerCase` transformation is removed .
    */
  def makeLang(langString: String): Sesame#Lang = langString.toLowerCase(Locale.ENGLISH)

  def fromLang(lang: Sesame#Lang): String = lang

  // graph traversal

  val ANY: Sesame#NodeAny = null

  implicit def toConcreteNodeMatch(node: Sesame#Node): Sesame#NodeMatch = node.asInstanceOf[Sesame#Node]

  def foldNodeMatch[T](nodeMatch: Sesame#NodeMatch)(funANY: => T, funConcrete: Sesame#Node => T): T =
    if (nodeMatch == null)
      funANY
    else
      funConcrete(nodeMatch.asInstanceOf[Sesame#Node])

  def find(graph: Sesame#Graph, subject: Sesame#NodeMatch, predicate: Sesame#NodeMatch, objectt: Sesame#NodeMatch): Iterator[Sesame#Triple] = {
    def sOpt: Option[Resource] =
      if (subject == null)
        Some(null)
      else
        foldNode(subject)(Some.apply, Some.apply, _ => None)
    def pOpt: Option[Sesame#URI] =
      if (predicate == null)
        Some(null)
      else
        foldNode(predicate)(Some.apply, _ => None, _ => None)
    val r = for {
      s <- sOpt
      p <- pOpt
    } yield {
      graph.filter(s, p, objectt).iterator.asScala
    }
    r getOrElse Iterator.empty
  }

  // graph union

  def union(graphs: Seq[Sesame#Graph]): Sesame#Graph = {
    graphs match {
      case Seq(x) => x
      case _ =>
        val graph = new LinkedHashModel
        graphs.foreach(g => getTriples(g) foreach { triple => graph.add(triple) })
        graph
    }
  }

  def diff(g1: Sesame#Graph, g2: Sesame#Graph): Sesame#Graph = {
    val graph = new LinkedHashModel
    getTriples(g1) foreach { triple =>
      if (!g2.contains(triple)) graph add triple
    }
    graph
  }

  // graph isomorphism
  def isomorphism(left: Sesame#Graph, right: Sesame#Graph): Boolean = {
    val leftNoContext = left.asScala.map(s => makeTriple(s.getSubject, s.getPredicate, s.getObject)).asJava
    val rightNoContext = right.asScala.map(s => makeTriple(s.getSubject, s.getPredicate, s.getObject)).asJava
    Models.isomorphic(leftNoContext, rightNoContext)
  }

  def graphSize(g: Sesame#Graph): Int = g.size()

  override def emptyQuadGraph: Sesame#QuadGraph = emptyGraph

  override def makeQuadGraph(it: Iterable[Sesame#Quad]): Sesame#QuadGraph = makeGraph(it)

  override def toQuadGraph(it: Iterable[Sesame#Triple], c: Option[Sesame#Node]): Sesame#QuadGraph = {
    c match {
      case Some(context) =>
        makeGraph(it.map(s => makeQuad(s.getSubject, s.getPredicate, s.getObject, context)))
      case _ =>
        makeGraph(it.map(s => makeQuad(s.getSubject, s.getPredicate, s.getObject)))
    }
  }

  override def makeDefaultGraph(graph: Sesame#Graph): Sesame#QuadGraph =
    makeGraph(getTriples(graph).map(tr => asTriple(tr)))

  override def getQuads(graph: Sesame#QuadGraph): Iterable[Sesame#Quad] = graph.iterator.asScala.toIterable

  override def makeQuad(s: Sesame#Node, p: Sesame#URI, o: Sesame#Node, c: Sesame#Node): Sesame#Quad =
    s match {
      case res:Resource=>
        c match {
          case context:Resource =>
            new ContextStatementImpl(res, p, o, context)
          case null =>  new ContextStatementImpl(res, p, o, null)
          case  _ => throw new RuntimeException("makeQuad: in Sesame context " + c.toString + " must be a either Sesame#URI or BlankNode")
        }
      case _=> throw new RuntimeException("makeQuad: in Sesame subject " + s.toString + " must be a either Sesame#URI or BlankNode")
    }

  override def makeQuad(s: Sesame#Node, p: Sesame#URI, o: Sesame#Node): Statement = makeTriple(s, p, o)

  override def fromQuad(quad: Statement): (Sesame#Node, Sesame#URI, Sesame#Node, Sesame#Node) = (quad.getSubject, quad.getPredicate, quad.getObject, quad.getContext)

  override def find(graph: Sesame#QuadGraph, subject: Sesame#NodeMatch, predicate: Sesame#NodeMatch, objectt: Sesame#NodeMatch, context: Sesame#NodeMatch): Iterable[Sesame#Quad] =
    predicate match {
      case p:URI =>
        subject match {
          case s:Resource =>
            context match {
              case c:Resource =>
                graph.filter(s, p, objectt, c).iterator().asScala.toIterable
              case  _ => throw new RuntimeException("find: in Sesame context " + context.toString + " must be a either Sesame#URI or BlankNode")
            }
          case _ => throw new RuntimeException("find: in Sesame subject " + subject.toString + " must be a either Sesame#URI or BlankNode")
        }
      case _ => throw new RuntimeException("find: in Sesame predicate " + predicate.toString + " must be a either Sesame#URI")
    }

  override def asTripleGraph(graph: Sesame#QuadGraph): Sesame#Graph = graph

  override def asTriple(quad: Sesame#Quad): Sesame#Triple =  makeTriple(quad.getSubject, quad.getPredicate, quad.getObject)

  override def asQuad(triple: Sesame#Triple, c: Option[Sesame#Node]): Sesame#Quad =  makeQuad(triple.getSubject, triple.getPredicate, triple.getObject, c.orNull)

  override def getNamedGraph(graph: Sesame#QuadGraph, c: Sesame#Node): Sesame#Graph =
    c match {
      case context:Resource =>
        graph.filter(null ,null, null, context)
      case  _ => throw new RuntimeException("getNamedGraph: in Sesame context " + c.toString + " must be a either Sesame#URI or BlankNode")
    }

  override def getDefaultGraph(graph: Sesame#QuadGraph): Sesame#Graph = graph.filter(null,null,null,null)

  override def isDefaultGraph(quad: Statement): Boolean = quad.getContext == null

}
