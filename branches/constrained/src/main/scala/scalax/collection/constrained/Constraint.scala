package scalax.collection.constrained

import scala.collection.Set
import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._
       
trait ConstraintCompanion[+CC[N, E[X] <: EdgeLikeIn[X]] <: Constraint[N,E]] {
  /** instantiates a user constraint */
  def apply [N, E[X] <: EdgeLikeIn[X]] (self: Graph[N,E]): CC[N,E]
}
/**
 * This template contains handler methods that are called by constrained graphs
 * whenever a constraint has been violated.
 * 
 * These methods must be overridden to get the handlers become active.
 * 
 * @author Peter Empen
 */
protected trait ConstraintHandlerMethods[N, E[X] <: EdgeLikeIn[X]]
{
  /** This handler is called whenever an addition violates the constraints.
   *  The provided default implementation is empty.
   */
  def onAdditionRefused(refusedNodes: Iterable[N],
                        refusedEdges: Iterable[E[N]],
                        graph:        Graph[N,E])
  {}
  /** This handler is called whenever a subtraction violates the constraints.
   *  The provided default implementation is empty.
   */
  def onSubtractionRefused (refusedNodes: Iterable[Graph[N,E]#NodeT],
                            refusedEdges: Iterable[Graph[N,E]#EdgeT],
                            graph:        Graph[N,E])
  {}
}
/**
 * This template contains all methods that constrained graphs call
 * to decide whether operations altering a mutable graph or operations
 * yielding a new graph from an immutable or mutable graph are valid.
 * 
 * Constraint methods are called on node/edge addition and subtraction.
 * Their names are prefixed by `may` or `commit`. They return `true`
 * if the operation should be carried out or `false` if it must be canceled.  
 * Member methods with the prefix `may` are called prior to the add or
 * subtract operations while those prefixed with `commit` allow to inspect the
 * would-be graph after the operation has taken place but has not yet been committed.
 * 
 * For performance reasons, implementations should prefer implementing pre-checking
 * meaning that only the `may*` methods would possibly return `false`.
 * But if it's necessary to check not only the operands but the whole would-be graph,
 * you should implement post-checking overriding the `commit*` methods.
 *
 * @define SELFGRAPH Use `self` to access the associated graph.
 * @define PREPOST This pre-check may be left out by letting it always return `true`
 *         and overriding the corresponding post-check `commit*` method.
 * @define SELFCOMMIT For immutable graphs, `self` maintains the state before the
 *         addition but for mutable graphs, it is already mutated to the required state.

 * @author Peter Empen
 */ 
trait ConstraintMethods[N, E[X] <: EdgeLikeIn[X]]
{
  /**
   * When extending `Constraint`, `self` will denote the attached constrained graph.
   * The factory methods of the companion object `scalax.collection.constrained.Graph`
   * initialize `self` to the correct graph instance.
   * When extending `Constrained`, `self` will denote `this` graph.
   */
  val self: Graph[N,E]
  /**
   * This pre-check is called on constructing a graph through its companion object.
   * It must return whether the graph is allowed to be populated with `nodes` and `edges`.
   * The default implementation calls `mayAdd` for each node and edge.
   * 
   * Note that nodes and edges coming from node/edge input streams are not checked.
   * So when utilizing streams the post check `commitAdd` must be served. 
   * 
   *  @param nodes the outer nodes the graph is to be populated with; nodes
   *         being edge ends may but need not be contained in `nodes`
   *  @param edges the outer edges the graph is to be populated with
   */
  def mayCreate(nodes: collection.Iterable[N],
                edges: collection.Iterable[E[N]]): Boolean =
    (nodes forall ((n: N)    => mayAdd(n))) &&
    (edges forall ((e: E[N]) => mayAdd(e)))
  /**
   * This pre-check must return whether the outer `node` is allowed to be added.
   * If `commitAdd` has been implemented, this method may always return `true`.
   * $PREPOST
   * $SELFGRAPH
   *
   * @param node to be added
   */
  def mayAdd(node: N): Boolean
  /**
   * This pre-check must return whether the outer `edge` is allowed to be added.
   * $PREPOST
   * $SELFGRAPH
   *
   * @param edge to be added
   */
  def mayAdd(edge: E[N]): Boolean
  /**
   * This pre-check must return whether the outer nodes and/or edges in `elems`
   * are allowed to be added. It is typically triggered by the `++` operation.
   * The default implementation calls `mayAdd(node)` or `mayAdd(edge)` element-wise.
   * As for most cases this won't be satisfactory you should provide
   * a domain-specific implementation.
   * $SELFGRAPH
   *
   * @param elems nodes and/or edges to be added possibly containing duplicates
   */
  def mayAdd   (elems: GraphParamIn[N,E]*): Boolean =
    elems forall { _ match {
        case node: NodeIn[N]   => mayAdd(node.value)
        case edge: EdgeIn[N,E] => mayAdd(edge.edge)  
      }
    }
  /**
   * This post-check must return whether `newGraph` should be committed.
   * $SELFGRAPH
   * $SELFCOMMIT
   *
   * @param newGraph the after-addition would-be graph waiting for commit
   * @param passedNodes nodes passed to the running add operation except those
   *        coming from node/edge input streams
   * @param passedEdges edges passed to the running add operation except those
   *        coming from edge input streams 
   * @param wasEmpty `true` if `self` was empty before the addition
   */
  def commitAdd(newGraph   : Graph[N,E],
                passedNodes: Iterable[N],
                passedEdges: Iterable[E[N]],
                wasEmpty   : Boolean): Boolean = true

  /**
   * This pre-check must return whether the outer `node` is allowed to be subtracted.
   * $PREPOST
   * $SELFGRAPH
   *
   * @param node to be subtracted
   * @param forced `true` for standard (ripple by `-`), `false` for gentle (by `-?`) removal 
   */
  def maySubtract(node: self.NodeT, forced: Boolean): Boolean
  /**
   * This pre-check must return whether the outer `edge` is allowed to be subtracted.
   * $PREPOST
   * $SELFGRAPH
   *
   * @param edge to be subtracted
   * @param simple `true` for standard (edge-only by `-`),
   *               `false` for ripple (by `-!`) removal 
   */
  def maySubtract(edge: self.EdgeT, simple: Boolean): Boolean
  /**
   * This pre-check must return whether the outer nodes and/or edges in `elems`
   * are allowed to be subtracted.  It is typically triggered by the `--` operation.
   * The default implementation calls `maySubtract(node, simple)` or
   * `maySubtract(edge, simple)` element-wise.
   * As for most cases this won't be satisfactory you should provide
   * a domain-specific implementation.
   * $SELFGRAPH
   *
   * @param nodes the inner nodes to be subtracted not necessarily including
   *              the ends of edges to be subtracted. Call allNodes to get the
   *              complete set of nodes to be subtracted.
   * @param edges the inner edges to be subtracted.
   * @param simple `true` for standard (edge-only by `-`),
   *               `false` for ripple (by `-!`) removal 
   */
  def maySubtract(nodes: => Set[self.NodeT],
                  edges: => Set[self.EdgeT],
                  simple:   Boolean): Boolean =
    (nodes forall (n => maySubtract(n, simple))) &&
    (edges forall (e => maySubtract(e, simple)))  
  /**
   * This post-check must return whether `newGraph` should be committed.
   * $SELFGRAPH
   * $SELFCOMMIT
   *
   * @param newGraph the after-subtraction would-be graph waiting for commit
   */
  def commitSubtract (newGraph: Graph[N,E],
                      passedNodes: Iterable[N],
                      passedEdges: Iterable[E[N]]): Boolean = true
  /** Consolidates all nodes of the arguments by adding edge ends to `passedNodes`. */
  def allNodes(passedNodes: Iterable[N],
               passedEdges: Iterable[E[N]]): Set[N] =
  {
    val nodes = collection.mutable.Set[N]() ++ passedNodes
    passedEdges foreach (nodes ++= _)
    nodes
  }
  /** Consolidates all nodes of the arguments by adding edge ends to `passedNodes`. */
  def allNodes(innerNodes: Set[self.NodeT],
               innerEdges: Set[self.EdgeT]): Set[self.NodeT] =
  {
    val nodes = collection.mutable.Set[self.NodeT]() ++ innerNodes
    innerEdges foreach (nodes ++= _)
    nodes
  }
}
/**
 * Template to be mixed in by any constrained graph class.
 * 
 * The user of the default dynamically constrained class
 * `scalax.collection.constrained.Graph` or its mutable counterpart does not
 * need to be concerned about this trait because it has been mixed in there.
 * She only needs to pass a factory object for her `Constraint` implementation
 * when calling `scalax.collection.constrained.Graph`.
 *    
 * It is the designer of any statically constrained graph class who will
 * have to mix in this trait in her constrained graph implementation.
 * 
 * @see ConstraintMethods
 * @author Peter Empen
 */
trait Constrained[N, E[X] <: EdgeLikeIn[X]]
  extends ConstraintMethods[N,E]
  with    ConstraintHandlerMethods[N,E]
{
}
/**
 * Template to be implemented and passed to a dynamically constrained graph class
 * by the user. Note that mutable state will be lost on any operation yielding a
 * new graph. Thus it is essential to either design classes inheriting from `Constraint`
 * in a pure immutable manner or taking internally care of whether the state has been lost. 
 *  
 * @param self denotes the attached constrained graph
 * @see ConstraintMethods
 * @author Peter Empen
 */
abstract class Constraint[N, E[X] <: EdgeLikeIn[X]] (override val self: Graph[N,E])
  extends ConstraintMethods[N,E]
  with    ConstraintHandlerMethods[N,E]
{
}
/**
 * The empty constraint letting through any addition or subtraction.
 */
class NoneConstraint[N, E[X] <: EdgeLikeIn[X]] (override val self: Graph[N,E])
  extends Constraint[N,E] (self)
  with    ConstraintHandlerMethods[N,E]
{
  override def mayAdd     (node:   N ) = true
  override def mayAdd     (edge: E[N]) = true
  override def maySubtract(node: self.NodeT, forced: Boolean) = true
  override def maySubtract(node: self.EdgeT, forced: Boolean) = true
}
object NoneConstraint extends ConstraintCompanion[NoneConstraint] {
  def apply [N, E[X] <: EdgeLikeIn[X]](self: Graph[N,E]) = new NoneConstraint[N,E](self) 
}