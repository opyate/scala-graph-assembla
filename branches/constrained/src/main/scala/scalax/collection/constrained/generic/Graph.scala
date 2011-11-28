package scalax.collection.constrained
package generic

import collection.Iterable
import collection.mutable.{Builder, ListBuffer}
import collection.generic.CanBuildFrom

import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn, GraphParamOut, NodeIn}

import mutable.GraphBuilder

trait GraphCompanion[+GC[N,E[X]<:EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,GC[N,E]]]
{
  type Coll = GC[_,Nothing]
  def empty[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint]): GC[N,E]
  def apply[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                      (elems:    GraphParamIn[N,E]*  ): GC[N,E]
  def from [N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint]  )
                                      (nodes:    Iterable[N],
                                       edges:    Iterable[E[N]]): GC[N,E]
  /** Produces a graph containing the results of some element computation a number of times.
   *  @param   nr  the number of elements to be contained in the graph.
   *  @param   elem the element computation.
   *  @return  A graph that contains the results of `nr` evaluations of `elem`.
   */
  def fill[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                     (nr: Int)
                                     (elem: => GraphParamIn[N,E] ): GC[N,E]
}
abstract class GraphFactory[GC[N,E[X]<:EdgeLikeIn[X]] <:
                            Graph[N,E] with GraphLike[N,E,GC[N,E]]]
  extends GraphCompanion[GC]
{
  override def apply[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                               (elems   : GraphParamIn[N,E]* ): GC[N,E]
    = (newBuilder[N,E](cFactory) ++= elems).result
  override def fill[N, E[X] <: EdgeLikeIn[X]](cFactory: ConstraintCompanion[Constraint])
                                             (nr: Int)
                                             (elem: => GraphParamIn[N,E]) =
  {
    val gB = newBuilder[N,E](cFactory).asInstanceOf[GraphBuilder[N,E,GC]]
    gB.sizeHint(nr)
    var i = 0
    while (i < nr) {
      gB += elem
      i += 1
    }
    gB.result
  }
  def newBuilder[N, E[X] <: EdgeLikeIn[X]]
                (cFactory: ConstraintCompanion[Constraint]): Builder[GraphParamIn[N,E], GC[N,E]]
    = new GraphBuilder[N,E,GC](this, cFactory)
  class GraphCanBuildFrom[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
    extends CanBuildFrom[Coll, GraphParamIn[N,E], GC[N,E]]
  {
    override def apply(from: Coll) = apply
    override def apply             = newBuilder[N,E](cFactory)
  }
}
abstract class MutableGraphFactory[GC[N,E[X]<:EdgeLikeIn[X]] <:
                                   mutable.Graph[N,E] with mutable.GraphLike[N,E,GC[N,E]]]
  extends GraphFactory[GC]
{
  override def newBuilder[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
    = new GraphBuilder[N,E,GC](this, cFactory)
}
abstract class ImmutableGraphFactory[GC[N,E[X]<:EdgeLikeIn[X]] <:
                                     immutable.Graph[N,E] with GraphLike[N,E,GC[N,E]]]
  extends GraphFactory[GC]
