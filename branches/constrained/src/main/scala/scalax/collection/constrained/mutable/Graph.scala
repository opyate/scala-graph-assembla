package scalax.collection.constrained
package mutable

import collection.{Set, Iterable}
import collection.generic.{CanBuildFrom, Growable, Shrinkable}
import collection.mutable.{Builder, Cloneable, ListBuffer, Set => MutableSet}

import scalax.collection.GraphEdge.{EdgeLike, EdgeCompanionBase}
import scalax.collection.GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn, GraphParamOut,
                                      GraphParamNode, NodeIn, NodeOut, EdgeIn, EdgeOut}
import scalax.collection.GraphTraversalImpl
import scalax.collection.mutable.BuilderImpl
import scalax.collection.io._
import scalax.collection.constrained.{Graph => CGraph}
import generic.GraphCompanion 

class GraphBuilder[N,
                   E[X] <: EdgeLikeIn[X],
                   GC[N,E[X] <: EdgeLikeIn[X]] <: scalax.collection.constrained.Graph[N,E] with
                                                  scalax.collection.constrained.GraphLike[N,E,GC[N,E]]]
      (graphFactory     : GraphCompanion     [GC],
       constraintFactory: ConstraintCompanion[Constraint]) 
  extends BuilderImpl[N,E,GC]
{
  def result: This = graphFactory.from(constraintFactory)(nodes, edges)
}
trait GraphLike[N,
                E[X] <: EdgeLikeIn[X],
               +This <: GraphLike[N, E, This] with Graph[N,E]]
	extends scalax.collection.mutable.GraphLike[N, E, This]
	with	  scalax.collection.constrained.GraphLike[N, E, This]
  with    Growable  [GraphParam[N,E]]
	with	  Shrinkable[GraphParam[N,E]] 
	with	  Cloneable [Graph[N,E]] 
  with    Mutable
{ this: This =>
  /** generic checked addition */
  protected def checkedAdd[G >: This]
            ( contained: => Boolean,
              mayAdd:    => Boolean,
              copy:      => G,
              nodes:     => Iterable[N],
              edges:     => Iterable[E[N]] ): This =
  { var graph = this
    if (! contained) {
      var handle = false
      if (checkSuspended || mayAdd) {
        graph = copy.asInstanceOf[This]
        if (! checkSuspended)
          if (! commitAdd(graph, nodes, edges, this.isEmpty)) {
            handle = true
            graph = this
          }
      } else handle = ! checkSuspended
      if (handle) onAdditionRefused(nodes, edges, this)
    }
    graph
  }
  override def + (node: N) = 
    checkedAdd (contained = nodes contains Node(node),
                mayAdd    = mayAdd(node),
                copy      = clone += node,
                nodes     = Set(node),
                edges     = Set.empty[E[N]])

  override def ++=(elems: TraversableOnce[GraphParam[N,E]]): this.type =
  { elems match {
      case elems: Iterable[GraphParam[N,E]] => 
        val p = new GraphParam.Partitions[N,E](elems)
        val inFiltered = p.toInParams.toSet.filter(elem => ! (this contains elem)).toSeq 
        var handle = false
        if (mayAdd(inFiltered: _*)) {
          val wasEmpty = this.isEmpty
          checkSuspended = true
            super.++=(elems)
          checkSuspended = false
          val (outerNodes, outerEdges) = (p.toOuterNodes, p.toOuterEdges)
          if (! commitAdd(this, outerNodes, outerEdges, wasEmpty)) {
            handle = true
            checkSuspended = true
              super.--=(allNodes(outerNodes, outerEdges) map (n => NodeIn(n)))
            checkSuspended = false
          }
        } else handle = true
        if (handle) onAdditionRefused(p.toOuterNodes, p.toOuterEdges, this)

      case _ => throw new IllegalArgumentException("Iterable expected")
    }
    this
  } 
}

import scalax.collection.constrained.generic.{GraphCompanion, GraphFactory, MutableGraphFactory}

trait Graph[N, E[X] <: EdgeLikeIn[X]]
	extends	scalax.collection.mutable.Graph[N,E]
  with    scalax.collection.constrained.Graph[N,E]
	with	  GraphLike[N, E, Graph[N,E]]
{
  override def empty: Graph[N,E] = Graph.empty[N,E](constraintFactory)
}
object Graph extends MutableGraphFactory[Graph]
{
  override def empty[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
    = DefaultGraphImpl.empty[N,E](cFactory)
  override def from [N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                               (nodes:   collection.Iterable[N],
                                                edges:   collection.Iterable[E[N]])
    = DefaultGraphImpl.from[N,E] (cFactory)(nodes, edges)
//	implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]: CanBuildFrom[Coll, GraphParamIn[N,E], Graph[N,E]] =
//		new GraphCanBuildFrom[N,E]
}
abstract class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]]
                               (iniNodes: Iterable[N]    = Set[N](),
                                iniEdges: Iterable[E[N]] = Set[E[N]]())
  extends Graph[N,E]
  with    AdjacencyListGraph[N,E,DefaultGraphImpl[N,E]]
  with    GraphTraversalImpl[N,E]
{
  protected val _nodes = new NodeSet 
  protected val _edges = new EdgeSet 
  initialize(iniNodes, iniEdges)

  @inline final override def empty = DefaultGraphImpl.empty(constraintFactory)
  @inline final override def clone(): this.type = super.clone.asInstanceOf[this.type]

  @SerialVersionUID(8082L)
  protected class NodeBase(override val value: N)
    extends super.NodeBase(value)
    with    InnerNodeImpl
    with    InnerNodeTraversalImpl
  type NodeT = NodeBase
  @inline final protected def newNode(n: N) = new NodeT(n)
}
object DefaultGraphImpl
  extends MutableGraphFactory[DefaultGraphImpl]
  with    GraphAuxCompanion  [DefaultGraphImpl]
{
  override def empty[N, E[X] <: EdgeLikeIn[X]]
                    (cFactory: ConstraintCompanion[Constraint])
    = from (cFactory)(Set.empty[N], Set.empty[E[N]])
  override def from [N, E[X] <: EdgeLikeIn[X]](cFactory: ConstraintCompanion[Constraint])
                                              (nodes: Iterable[N],
                                               edges: Iterable[E[N]]): DefaultGraphImpl[N,E] =
  { val existElems = nodes.nonEmpty || edges.nonEmpty 
    if (existElems) {
      val emptyGraph = empty[N,E](cFactory)
      val constraint = cFactory(emptyGraph)
      if (! constraint.mayCreate(nodes, edges)) {
        constraint onAdditionRefused (nodes, edges, emptyGraph) 
        return emptyGraph
      }
    }
    val newGraph = new UserConstrainedGraphImpl[N,E](cFactory)(nodes, edges)
    if (existElems) {
      val emptyGraph = empty[N,E](cFactory)
      val constraint = cFactory(emptyGraph)
      if (constraint.commitAdd(newGraph, nodes, edges, true))
        newGraph
      else {
        constraint.onAdditionRefused(nodes, edges, newGraph)
        emptyGraph
      }
    } else
      newGraph
  }
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (cFactory: ConstraintCompanion[Constraint])
     (nodeStreams: Seq[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      iniNodes:    Iterable[N]             = Seq.empty[N],
      edgeStreams: Seq[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      iniEdges:    Iterable[E[N]]          = Seq.empty[E[N]]): DefaultGraphImpl[N,E] =
  {
    if (iniNodes.nonEmpty || iniEdges.nonEmpty) {
      val emptyGraph = empty[N,E](cFactory)
      val constraint = cFactory(emptyGraph)
      if (! constraint.mayCreate(iniNodes, iniEdges)) {
        constraint onAdditionRefused (iniNodes, iniEdges, emptyGraph) 
        return emptyGraph
      }
    }
    val newGraph = new UserConstrainedGraphImpl[N,E](cFactory)() {
      from(nodeStreams, iniNodes, edgeStreams, iniEdges)
    }
    if (newGraph.commitAdd(newGraph, iniNodes, iniEdges, true))
      newGraph
    else {
      newGraph.onAdditionRefused(iniNodes, iniEdges, newGraph)
      empty[N,E](cFactory)
    }
  }
  //implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]: CanBuildFrom[Coll, GraphParamIn[N,E], DefaultGraphImpl[N,E]] =
  //  new GraphCanBuildFrom[N,E]
}
class UserConstrainedGraphImpl[N, E[X] <: EdgeLikeIn[X]]
                              (cFactory: ConstraintCompanion[Constraint])
                              (iniNodes: Iterable[N]    = Set.empty[N],
                               iniEdges: Iterable[E[N]] = Set.empty[E[N]])
  extends DefaultGraphImpl    [N,E](iniNodes, iniEdges)
  with    UserConstrainedGraph[N,E]
{
  final override val self = this
  final override val constraintFactory = cFactory
  final override val constraint  = cFactory(this)
}
