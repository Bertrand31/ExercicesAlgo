import scala.annotation.tailrec

object HuffmanEncoding extends App {

  trait Tree(count: Int):
    def getValue: Int = this.count

  final case class Node(sum: Int, left: Tree, right: Tree) extends Tree(count = sum)
  final case class Leaf(char: Char, count: Int) extends Tree(count = count)

  private def buildTree(sortedCharsCounts: List[(Char, Int)]): Tree =
    val (char, count) :: tail = sortedCharsCounts: @unchecked
    if tail.isEmpty then Leaf(char, count)
    else
      val newLeaf = Leaf(char, count)
      val newTree = buildTree(tail)
      val newTreeValue = newTree.getValue
      val localSum = newTreeValue + count
      if newTreeValue > count then Node(localSum, newLeaf, newTree)
      else Node(localSum, newTree, newLeaf)

  private def encodeChar(tree: Tree, char: Char): Option[String] =
    tree match
      case Node(_, left, right) =>
        encodeChar(left, char).map('0' +: _)
          .orElse(encodeChar(right, char).map('1' +: _))
      case Leaf(`char`, _) => Some("")
      case Leaf(_, _) => None
        

  def encode(str: String): (Tree, String) =
    val charsMap = str.groupMapReduce(identity)(_ => 1)(_ + _)
    val charsCountsSorted = charsMap.toList.sortBy(-_._2)
    val encodingTree = buildTree(charsCountsSorted)
    val encodedString = str.map(encodeChar(encodingTree, _).get).mkString
    (encodingTree, encodedString)

  @tailrec
  private def decodeChars(treeRoot: Tree, currentTree: Tree, chars: List[Char], soFar: List[Char]): List[Char] =
    currentTree match
      case Leaf(c, _) =>
        if chars.isEmpty then (c :: soFar).reverse
        else decodeChars(treeRoot, treeRoot, chars, c :: soFar)
      case Node(_, left, right) =>
        val head :: tail = chars: @unchecked
        if head == '0' then decodeChars(treeRoot, left, tail, soFar)
        else decodeChars(treeRoot, right, tail, soFar)

  def decode(tree: Tree, str: String): String =
    decodeChars(tree, tree, encoded.toList, List.empty[Char]).mkString

  val sample = "ABRACADABRA"
  val (tree, encoded) = encode(sample)
  val decoded = decode(tree, encoded)
  assert(decoded == sample)
}
