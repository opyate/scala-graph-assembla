package scalax.collection.constrained
package mutable

import collection.Set
import collection.mutable.{HashSet, HashMap, Set => MutableSet}

import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.mutable.{AdjacencyListGraph => SimpleAdjacencyListGraph}

/**
 * Implements an adjacency list based graph representation.
 * 
 * An adjacency list based representation speeds up traversing a graph along its paths
 * by storing the list of connecting edges to each node.
 *   
 * @author Peter Empen
 */
trait AdjacencyListGraph[N,
                         E[X] <: EdgeLikeIn[X],
                        +This <: AdjacencyListGraph[N, E, This] with Graph[N,E]]
	extends GraphLike[N, E, This]
  with    SimpleAdjacencyListGraph[N,E,This]
{ this: This =>
  override protected def initialize(nodes: Iterable[N],
                                    edges: Iterable[E[N]] ) {
    checkSuspended = true
    super.initialize(nodes, edges)
    checkSuspended = false
  }
  @SerialVersionUID(8083L)
  class NodeSet extends super.NodeSet
  {
    override def add(node: NodeT) = { 
      var handle = false
      if (coll.contains(node)) false
      else {
        if (checkSuspended || mayAdd(node)) {
          val wasEmpty = isEmpty
          coll += (node -> emptyEdges)
          if (! checkSuspended)
            if (! commitAdd(AdjacencyListGraph.this,
                            Set(node.value), Set.empty[E[N]], wasEmpty))
            { handle = true
              coll -= node
            }
        } else handle = ! checkSuspended
        if (handle)
          onAdditionRefused(Set(node), Set.empty[E[N]], AdjacencyListGraph.this)
      }
      ! handle
    }
  }
  @SerialVersionUID(8084L)
  class EdgeSet extends super.EdgeSet
  {
    override def add(edge: EdgeT) = {
      var handle = false
      if (checkSuspended ||
           (edge forall {n => if (_nodes find n isDefined) true
                              else mayAdd(n.value)}) &&
           mayAdd(edge.toEdgeIn))
      {
          val wasEmpty = isEmpty
          super.add(edge)
      } else handle = ! checkSuspended
      if (handle)
        onAdditionRefused(Set.empty[N], Set(edge.toEdgeIn), AdjacencyListGraph.this)
      ! handle
    }
  }
}