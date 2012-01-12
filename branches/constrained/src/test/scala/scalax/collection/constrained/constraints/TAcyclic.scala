package scalax.collection.constrained

import org.scalatest.{Suite, Suites}
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import PreCheckFollowUp._
import generic.GraphFactory

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TAcyclicRootTest
  extends Suites(
      new TAcyclic[immutable.Graph](immutable.Graph),
      new TAcyclic[  mutable.Graph](  mutable.Graph))
  with ShouldMatchers
{
  import mutable.Graph

  def test_mutableAsyclic {
    import Simple._
    val g = Graph(Asyclic)(1~>2,3~>4)
  }
}

class TAcyclic [CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC[N,E]]]
    (val factory: GraphFactory[CC])
  extends Suite
  with    ShouldMatchers
{
  def test_0(info : Informer) {
    info("factory = " + factory.getClass)
  }
  def test_addDiEdge {
    import Simple._
    val g = Graph(Asyclic)(1~>2, 2~>3)
    evaluating { g + 3~>1 } should produce [CyclicException]
    g + 3~>4 should have size (7)
  }
  def test_addDiHyperEdge {
    import Simple._
    val g = Graph[Int,HyperEdge](Asyclic)(1~>2~>3, 2~>3~>4)
    evaluating { g + 4~>2 } should produce [CyclicException]
    g + 1~>4 should have size (7)
  }
}
object Simple {
  /** Constrains the graph to be acyclic. All edges of the underlying graph must
   *  be directed and loop-free. Note that directed hyperedges are also covered.
   */
  class Asyclic[N, E[X] <: EdgeLikeIn[X]] (override val self: Graph[N,E])
    extends Constraint[N,E] (self)
    with    ConstraintHandlerMethods[N,E]
  {
    private def dontAdd = "' must not be added."
    def validDiEdge(edge: E[N]): (DiHyperEdgeLike[N]) = edge match {
      case diEdge: DiHyperEdgeLike[N] =>
        if ((diEdge tail) exists (_ == diEdge.source))
          throw new CyclicException("Looping edge '" + diEdge + dontAdd)
        else
          diEdge
      case _ =>
        throw new CyclicException("Directed edge '" + edge + dontAdd)
    }
    def withValidDiEdge(edge: E[N])(check: (DiHyperEdgeLike[N]) => Boolean) =
      check(validDiEdge(edge))

    // difficult to say so postpone it until post-check
    override def preCreate(nodes: collection.Iterable[N],
                           edges: collection.Iterable[E[N]]) = PreCheckResult(PostCheck)
    // an unconnected node would have a degree of 0
    def preAdd(node: N) = PreCheckResult(PostCheck)
    /*
     * When inserting an edge with a source contained in the graph
     * a cycle is produced if there exists a target node of this edge such that
     * there is a path from the target node to the source node  
     */
    def preAdd(edge: E[N]) = PreCheckResult.complete(
      withValidDiEdge(edge) { diEdge =>
        val outerSource = diEdge.source
        self find outerSource map (innerSource =>
          (diEdge tail) forall { outerTarget =>
            self find outerTarget map (innerTarget =>
              ! (innerTarget hasSuccessor innerSource) // would produce a cycle  
            ) getOrElse true // target is a new node
          }
        ) getOrElse true // source is a new node
      })
    // difficult to say so postpone it until post-check
    override def preAdd(elems: GraphParamIn[N,E]*) = PreCheckResult(PostCheck) // TODO
    // inspecting the would-be graph is much easier
    override def postAdd (newGraph: Graph[N,E],
                          passedNodes: Iterable[N],
                          passedEdges: Iterable[E[N]],
                          preCheck   : PreCheckResult) = 
    { true // TODO
    }
    override def onAdditionRefused( refusedNodes: Iterable[N],
                                    refusedEdges: Iterable[E[N]],
                                    graph:        Graph[N,E]) =
    { throw new CyclicException("Addition refused: " +
                "nodes = " + refusedNodes + ", " +
                "edges = " + refusedEdges)
    }
    def preSubtract(node: self.NodeT, forced: Boolean) = PreCheckResult(PostCheck)
    def preSubtract(edge: self.EdgeT, forced: Boolean) = PreCheckResult(PostCheck)
  }
  object Asyclic extends ConstraintCompanion[Asyclic] {
    def apply [N, E[X] <: EdgeLikeIn[X]] (self: Graph[N,E]) = new Asyclic[N,E] (self)
  }
  class CyclicException(msg: String) extends IllegalArgumentException(msg)
}