import scala.collection.immutable.Queue

sealed trait Tree
final case class Node(left: Tree, right: Tree, value: Int) extends Tree
final case class Leaf(value: Int) extends Tree

object TreeTraversals:
  
  def dfs(tree: Tree): LazyList[Int] =
    tree match
      case Node(left, right, value) => value #:: (dfs(left) ++ dfs(right))
      case Leaf(value) => LazyList(value)

  def bfs(tree: Tree): LazyList[Int] =
    def traverse(queue: Queue[Tree], soFar: LazyList[Int]): LazyList[Int] =
      if queue.isEmpty then soFar
      else
        val (tree, qTail) = queue.dequeue
        tree match
          case Leaf(value) =>
            traverse(qTail, value #:: soFar)
          case Node(left, right, value) =>
            traverse(qTail.enqueue(left).enqueue(right), value #:: soFar)
    traverse(Queue(tree), LazyList.empty) 

object TreeTraversalsApp extends App:
  
  val sample = Node(
    value = 0,
    left = Node(
      value = 1,
      left = Node(
        value = 2,
        left = Leaf(3),
        right = Leaf(4),
      ),
      right = Leaf(5),
    ),
    right = Leaf(6),
  )
  {
    val res = TreeTraversals.bfs(sample)
    println(res.toList)
  }
  {
    val res = TreeTraversals.dfs(sample)
    println(res.toList)
  }
