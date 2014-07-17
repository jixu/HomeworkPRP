/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op: Operation => root ! op
    case GC =>
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context become garbageCollecting(newRoot)
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyFinished =>
      root ! PoisonPill
      root = newRoot
      while (!pendingQueue.isEmpty) {
        val p = pendingQueue.dequeue
        root ! p._1
        pendingQueue = p._2
      }
      pendingQueue = Queue.empty[Operation]
      context become normal
    case op: Operation => pendingQueue = pendingQueue.enqueue(op)
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Contains(requester, id, element) =>
      if (elem == element) {
        requester ! ContainsResult(id, !removed)
      } else if (elem > element) {
        subtrees.get(Left) match {
          case Some(ref) => ref ! Contains(requester, id, element)
          case None => requester ! ContainsResult(id, false)
        }
      } else {
        subtrees.get(Right) match {
          case Some(ref) => ref ! Contains(requester, id, element)
          case None => requester ! ContainsResult(id, false)
        }
      }
    case Insert(requester, id, element) =>
      if (elem == element) {
        removed = false
        requester ! OperationFinished(id)
      } else if (elem > element) {
        subtrees.get(Left) match {
          case Some(ref) => ref ! Insert(requester, id, element)
          case None =>
            subtrees = subtrees + (Left -> context.actorOf(BinaryTreeNode.props(element, false)))
            requester ! OperationFinished(id)
        }
      } else {
        subtrees.get(Right) match {
          case Some(ref) => ref ! Insert(requester, id, element)
          case None =>
            subtrees = subtrees + (Right -> context.actorOf(BinaryTreeNode.props(element, false)))
            requester ! OperationFinished(id)
        }
      }
    case Remove(requester, id, element) =>
      if (elem == element) {
        removed = true
        requester ! OperationFinished(id)
      } else if (elem > element) {
        subtrees.get(Left) match {
          case Some(ref) => ref ! Remove(requester, id, element)
          case None => requester ! OperationFinished(id)
        }
      } else {
        subtrees.get(Right) match {
          case Some(ref) => ref ! Remove(requester, id, element)
          case None => requester ! OperationFinished(id)
        }
      }
    case CopyTo(ref) =>
      subtrees foreach { p =>
        p._2 ! CopyTo(ref)
      }
      if (!removed) {
        ref ! Insert(self, elem, elem)
        context become copying(subtrees.values.toSet, false)
      } else {
        context become copying(subtrees.values.toSet, true)
      }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case CopyFinished =>
      sender ! PoisonPill
      if ((expected-sender).isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
      }
      else {
        context become copying(expected-sender, insertConfirmed)
      }
    case OperationFinished(id) =>
      if (expected.isEmpty) {
        context.parent ! CopyFinished
      } else {
        context become copying(expected, true)
      }
  }

}
