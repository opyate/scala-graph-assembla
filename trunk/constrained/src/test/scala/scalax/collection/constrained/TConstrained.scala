package scalax.collection.constrained

import org.scalatest.{Suite, Suites}
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Ignore

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import generic.{GraphCompanion, GraphFactory}

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TConstrainedRootTest
  extends Suites(
      new TConstrained[immutable.Graph](immutable.Graph),
      new TConstrained[  mutable.Graph](  mutable.Graph))
  with ShouldMatchers
{
  import mutable.Graph

  def test_mutableEvenNode {
    import UserConstraints.EvenNode
    val g = Graph(EvenNode)(1)
    g should be ('isEmpty)
    (g += 2) should have size (1)
    (g += 3) should have size (1)
    (g ++= List(1,4))   should have size (1)
    (g ++= List(2,4,6)) should have size (3)
  }
  def test_mutableMinDegree {
    import UserConstraints.{MinDegree_2, MinDegreeException}
    val g = Graph.empty[Int,UnDiEdge](MinDegree_2)
    evaluating { g ++= List(2,3,4)         } should produce [MinDegreeException]
    evaluating { g ++= List(1~2, 1~3, 2~4) } should produce [MinDegreeException]
    (g ++= List(1~2, 1~3, 2~3)) should have size (6)
  }
}

class TConstrained [+CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC[N,E]]]
    (val factory: GraphCompanion[CC])
  extends Suite
  with    ShouldMatchers
{
  def test_0(info : Informer) {
    info("factory = " + factory.getClass)
  }
  def test_EvenNode {
    import UserConstraints.EvenNode
    val g = factory(EvenNode)(1,2,3,4)
    g should be ('isEmpty)
    g + 5 contains 5 should be (false)
    g + 6 contains 6 should be (true)
    (g ++ List[NodeIn[Int]](1,2,3)) should be ('isEmpty)
    (g ++ List[NodeIn[Int]](2,4,6)) should have size (3)
  }
  def test_MinDegree {
    import UserConstraints.{MinDegree_2, MinDegreeException}
    evaluating {
      factory(MinDegree_2)(1, 2, 3~4) } should produce [MinDegreeException]
    val g = factory.empty[Int,UnDiEdge](MinDegree_2)
    evaluating { g.onAdditionRefused(null, null, g) } should produce [MinDegreeException]
    evaluating { g + 1~2                            } should produce [MinDegreeException]
    (g ++ List(1~2, 1~3, 2~3)) should have size (6)
  }
}
object UserConstraints {
  /* Constrains nodes to even numbers for N = Int doing pre-checks only. */
  class EvenNode[N, E[X] <: EdgeLikeIn[X]] (override val self: Graph[N,E])
    extends Constraint[N,E] (self)
    with    ConstraintHandlerMethods[N,E]
  {
    def mayAdd(node: N) = node match { case i:Int => i % 2 == 0;
                                       case _ => true }
    def mayAdd(edge: E[N]) = true
    def maySubtract(node: self.NodeT, forced: Boolean) = true
    def maySubtract(edge: self.EdgeT, forced: Boolean) = true
  }
  object EvenNode extends ConstraintCompanion[EvenNode] {
    def apply [N, E[X] <: EdgeLikeIn[X]] (self: Graph[N,E]) =
      new EvenNode[N,E] (self) 
  }
  /* Constrains the graph to have a minimal degree of `min` utilizing post-checks.
   */
  abstract class MinDegree[N, E[X] <: EdgeLikeIn[X]] (override val self: Graph[N,E])
    extends Constraint[N,E] (self)
    with    ConstraintHandlerMethods[N,E]
  {
    val min: Int

    // difficult to say so postpone it until post-check
    override def mayCreate(nodes: collection.Iterable[N],
                           edges: collection.Iterable[E[N]]) = true
    // an unconnected node would have a degree of 0
    def mayAdd(node: N) = false
    // any edge ends not yet contained in the graph would have a degree of 1
    def mayAdd(edge: E[N]) = edge forall (self contains _)
    // difficult to say so postpone it until post-check
    override def mayAdd(elems: GraphParamIn[N,E]*) = true
    // inspecting the would-be graph is much easier
    override def commitAdd (newGraph: Graph[N,E],
                            passedNodes: Iterable[N],
                            passedEdges: Iterable[E[N]],
                            wasEmpty: Boolean) = 
    { allNodes(passedNodes, passedEdges) forall (
        n => (newGraph get n).degree >= min)
    }
    override def onAdditionRefused( refusedNodes: Iterable[N],
                                    refusedEdges: Iterable[E[N]],
                                    graph:        Graph[N,E])
    { throw new MinDegreeException("Addition refused: " +
                "nodes = " + refusedNodes + ", " +
                "edges = " + refusedEdges)
    }
    def maySubtract(node: self.NodeT, forced: Boolean) = true
    def maySubtract(edge: self.EdgeT, forced: Boolean) = true
  }
  object MinDegree_2 extends ConstraintCompanion[MinDegree] {
    def apply [N, E[X] <: EdgeLikeIn[X]] (self: Graph[N,E]) =
      new MinDegree[N,E] (self) {
        val min = 2
      }
  }
  class MinDegreeException(msg: String) extends IllegalArgumentException(msg)
}