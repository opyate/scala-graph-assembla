package custom

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers

import scalax.collection.Graph
import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TExtByImplicitTest
	extends	Suite
	with	  ShouldMatchers
{
  def test_newGraphMethod {
    /* extension ----------------------
     * `ok` is your new method
     */
    final class ExtGraph[N, E[X] <: EdgeLikeIn[X]](g: Graph[N,E]) {
      def ok = g.order + g.graphSize == g.size
    }
    implicit def gToExtG[N, E[X] <: EdgeLikeIn[X]](g: Graph[N,E]) =
      new ExtGraph[N,E](g)

    // test of extension --------------
    Graph.empty.ok should be (true)
    Graph(1, 2).ok should be (true)
    Graph(1~>2).ok should be (true)
  }
}