package scalax.collection.constrained

import collection.{Set, SetLike}

import scalax.collection.GraphPredef.{EdgeLikeIn, GraphParam,
       GraphParamIn, GraphParamOut, seqToGraphParam, NodeIn, NodeOut, EdgeIn, EdgeOut}
import scalax.collection.GraphEdge.{EdgeLike, EdgeCompanionBase}
import scalax.collection.{GraphLike => SimpleGraphLike, Graph => SimpleGraph}
import scalax.collection.io._

/**
 * A template trait for graphs.
 * 
 * This trait provides the common structure and operations of immutable graphs independently of its representation.
 * 
 * If `E` inherits `DirectedEdgeLike` the graph is directed, otherwise it is undirected or mixed.
 * 
 * @tparam N    the user type of the nodes (vertices) in this graph.
 * @tparam E    the kind of the edges (links) in this graph.
 * @tparam This the type of the graph itself.
 *
 * @author Peter Empen
 */
trait GraphLike[N,
                E[X]  <: EdgeLikeIn[X],
                +This <: GraphLike[N,E,This] with Set[GraphParam[N,E]] with Graph[N,E]]
  extends SimpleGraphLike[N,E,This]
  with    Constrained[N,E]
{ this: This =>
  /** This flag is used to prevent constraint checking for single additions and
   * subtractions triggered by a multiple addition/subtraction such as `++=`.
   */
  protected var checkSuspended = false

  private def copyThis(nodes: Iterable[N],
                       edges: Iterable[E[N]]): This = this match {
    case _: Mutable =>   mutable.Graph.from(constraintFactory)(nodes, edges).asInstanceOf[This]
    case _          => immutable.Graph.from(constraintFactory)(nodes, edges).asInstanceOf[This]
  }
  override def ++(elems: TraversableOnce[GraphParam[N,E]]): this.type =
  { var graph = this.asInstanceOf[This]
    elems match {
      case elems: Iterable[GraphParam[N,E]] => 
        val p = new GraphParam.Partitions[N,E](elems filter (elm => !(this contains elm)))
        val inFiltered = p.toInParams.toSet.toSeq
        val (outerNodes, outerEdges) = (p.toOuterNodes, p.toOuterEdges)
        var handle = false
        if (mayAdd(inFiltered: _*)) {
          val wasEmpty = this.isEmpty
          graph = copyThis(outerNodes, outerEdges)
          if (! commitAdd(graph, outerNodes, outerEdges, wasEmpty)) {
            handle = true
            graph = this.asInstanceOf[This]
          }
        } else handle = true
        if (handle) onAdditionRefused(outerNodes, outerEdges, graph)

      case _ => throw new IllegalArgumentException("Iterable expected")
    }
    graph.asInstanceOf[this.type]
  } 
  override def --!(elems: Iterable[GraphParam[N,E]]) =
  { var graph = this.asInstanceOf[This]

    lazy val p = new GraphParam.Partitions[N,E](elems)
    lazy val (outerNodes, outerEdges) = (p.toOuterNodes.toSet, p.toOuterEdges.toSet)
    def innerNodes =
       (outerNodes.view map (this find _) filter (_.isDefined) map (_.get) force).toSet
    def innerEdges =
       (outerEdges.view map (this find _) filter (_.isDefined) map (_.get) force).toSet

    type C_NodeT = self.NodeT
    type C_EdgeT = self.EdgeT
    var handle = false
    if (maySubtract(innerNodes.asInstanceOf[Set[C_NodeT]],
                    innerEdges.asInstanceOf[Set[C_EdgeT]], false)) {
      graph = (graph /: elems)(_ -! _) // TODO optimize
      if (! commitSubtract(graph, outerNodes, outerEdges)) {
        handle = true
        graph = this.asInstanceOf[This]
      }
    } else handle = true
    if (handle) onAdditionRefused(outerNodes, outerEdges, graph)

    graph.asInstanceOf[this.type]
  }
}

// ----------------------------------------------------------------------------
import collection.generic.CanBuildFrom

import generic.{GraphCompanion, GraphFactory}
/**
 * A trait for dynamically constrained graphs.
 * 
 * @tparam N    the type of the nodes (vertices) in this graph.
 * @tparam E    the kind of the edges in this graph. 
 *
 * @author Peter Empen
 */
trait Graph[N, E[X] <: EdgeLikeIn[X]]
  extends Set        [GraphParam[N,E]]
  with    SimpleGraph[N,E]
  with    GraphLike  [N,E,Graph[N,E]]
{
  override def empty: Graph[N,E] = Graph.empty[N,E](constraintFactory)
  val constraintFactory: ConstraintCompanion[Constraint]
}
/**
 * Default factory for constrained graphs.
 * Graph instances returned from this factory will be immutable.
 * 
 * @author Peter Empen
 */
object Graph
  extends GraphFactory[Graph]
  with    GraphAuxCompanion[Graph]
{
  override def newBuilder[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
    = immutable.Graph.newBuilder[N,E](cFactory)
  override def empty     [N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
    = immutable.Graph.empty[N,E](cFactory)
  override def from      [N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                                    (nodes: Iterable[N],
                                                     edges: Iterable[E[N]])
    = immutable.Graph.from[N,E](cFactory)(nodes, edges)
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (cFactory: ConstraintCompanion[Constraint])
     (nodeStreams: Seq[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]             = Seq.empty[N],
      edgeStreams: Seq[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]          = Seq.empty[E[N]]): Graph[N,E] =
    immutable.Graph.fromStream[N,E](cFactory)(
                                    nodeStreams, nodes, edgeStreams, edges)
//  implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]: CanBuildFrom[Coll, GraphParamIn[N,E], Graph[N,E]] =
//    new GraphCanBuildFrom[N,E]
}
trait UserConstrainedGraph[N, E[X] <: EdgeLikeIn[X]]
  extends Graph[N,E]
{
  val constraint: Constraint[N,E]
 /*
   * delegating from Constrained to Constraint;
   * asInstanceOf is safe because 'this' is set to 'constraint.self' 
   */
  private type C_NodeT = constraint.self.NodeT
  private type C_EdgeT = constraint.self.EdgeT

  override def mayCreate(nodes: collection.Iterable[N],
                         edges: collection.Iterable[E[N]]) =
                                    constraint mayCreate (nodes, edges)
  override def mayAdd(node: N   ) = constraint mayAdd node
  override def mayAdd(edge: E[N]) = constraint mayAdd edge
  override def mayAdd(elems: GraphParamIn[N,E]*) = constraint mayAdd (elems: _*)
  override def commitAdd (newGraph   : scalax.collection.constrained.Graph[N,E],
                          passedNodes: Iterable[N],
                          passedEdges: Iterable[E[N]],
                          wasEmpty   : Boolean) =
    constraint commitAdd (newGraph, passedNodes, passedEdges, wasEmpty)

  override def maySubtract (node: self.NodeT, forced: Boolean) =
    constraint maySubtract (node.asInstanceOf[C_NodeT], forced)
  override def maySubtract (edge: self.EdgeT, simple: Boolean) =
    constraint maySubtract (edge.asInstanceOf[C_EdgeT], simple)
  override def maySubtract (nodes: => Set[self.NodeT],
                            edges: => Set[self.EdgeT], simple: Boolean) =
    constraint maySubtract (nodes.asInstanceOf[Set[C_NodeT]],
                            edges.asInstanceOf[Set[C_EdgeT]],
                            simple)
  override def commitSubtract(newGraph   : scalax.collection.constrained.Graph[N,E],
                              passedNodes: Iterable[N],
                              passedEdges: Iterable[E[N]]) =
    constraint commitSubtract (newGraph, passedNodes, passedEdges)

  override def onAdditionRefused   (refusedNodes: Iterable[N],
                                    refusedEdges: Iterable[E[N]],
                                    graph:        Graph[N,E]) {
    constraint onAdditionRefused   (refusedNodes, refusedEdges, graph)
  }
  override def onSubtractionRefused(refusedNodes: Iterable[Graph[N,E]#NodeT],
                                    refusedEdges: Iterable[Graph[N,E]#EdgeT],
                                    graph:        Graph[N,E]) {
    constraint onSubtractionRefused(refusedNodes.asInstanceOf[Iterable[C_NodeT]],
                                    refusedEdges.asInstanceOf[Iterable[C_EdgeT]],
                                    graph)
  }
}
