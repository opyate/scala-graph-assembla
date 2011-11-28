package scalax.collection
/**
 * Contains traits needed to implement user constraints and constrained graphs.
 * 
 * Graphs may be constrained dynamically or statically.
 *
 * ''Dynamically constrained'' means that the graph class excepts an implementation
 * of this trait at instantiation time and delegates all calls to `ConstraintMethods`
 * methods to the corresponding methods of the passed implementation of this trait.
 * The immutable and mutable factories `Graph` in this package yield dynamically
 * constrained graphs.
 * 
 * To make use of dynamically constrained graphs you must provide an implementation
 * of `Constraint` and a companion object extending `ConstraintCompanion`. To initialize
 * a graph with your constraint call the graph factory methods of the `constraint`
 * package passing your companion object.  
 *
 * ''Statically constrained'' means that the graph class directly implements
 * the methods declared in `ConstraintMethods`. For instance, DAG is
 * a statically constrained graph implementation.
 * 
 * @author Peter Empen
 */
package object constrained {
}
