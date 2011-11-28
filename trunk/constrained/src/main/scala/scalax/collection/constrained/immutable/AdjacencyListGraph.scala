package scalax.collection.constrained
package immutable

import scala.collection.generic.CanBuildFrom

import scalax.collection.GraphPredef.{EdgeLikeIn, NodeIn, EdgeIn, GraphParamIn}
import scalax.collection.constrained.{Constrained, Constraint}
import scalax.collection.immutable.{AdjacencyListGraph => SimpleAdjacencyListGraph}

trait AdjacencyListGraph[
      N,
      E[X] <: EdgeLikeIn[X],
     +This <: AdjacencyListGraph[N,E,This] with Graph[N,E]]  
  extends GraphLike[N,E,This]
  with    SimpleAdjacencyListGraph[N,E,This]
{ this: This =>
  override protected def initialize(nodes: Iterable[N],
                                    edges: Iterable[E[N]] ) {
    checkSuspended = true
    super.initialize(nodes, edges)
    checkSuspended = false
  }
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
    checkedAdd (contained = _nodes contains Node(node),
                mayAdd    = mayAdd(node),
                copy      = copy(_nodes.toNodeInSet.toBuffer += node,
                                 _edges.toEdgeInSet),
                nodes     = Set(node),
                edges     = Set.empty[E[N]])

  override protected def +#(edge: E[N]) =
    checkedAdd (contained = _edges contains Edge(edge),
                mayAdd    = mayAdd(edge),
                copy      = copy(_nodes.toNodeInSet,
                                 _edges.toEdgeInSet.toBuffer += edge),
                nodes     = Set.empty[N],
                edges     = Set(edge))

}