object T {

  /**
    * trait tree represents a binary tree
    */
  trait Tree {
    def depth: Int = this match {
      case Leaf() => 0
      case Node(v, l, r) => math.max(l.depth, r.depth)
    }
  }

  /**
    * case class Node is a non null Tree
    * @param value: the value of the node
    * @param left: left child
    * @param right: right child
    */
  case class Node(value: Int, left: Tree, right: Tree) extends Tree {}

  /**
    * case class Leaf is a null Tree
    */
  case class Leaf() extends Tree{}

  /**
    * constructs a Tree from an array of nodes
    * @param nodes: an array to construct the tree from. The i-th element of the
    *             array is a pair of values first one of which is the left child
    *             of the Node given as a number where it is stored in the array,
    *             second one is the right child in the same logic. If a node has
    *             no children, -1 is written
    * @param i: the number of the node being constructed
    * @return a tree constructed in a described way
    */
  def makeTree(nodes: Array[(Int, Int)], i: Int): Tree = {
    val (l, r) = nodes(i - 1)
    Node(i, if(l == -1) Leaf() else makeTree(nodes, l),
      if (r == -1) Leaf() else makeTree(nodes, r))
  }

  /**
    * traverses a binary tree into a list from left to right
    * @param t: the tree to be traversed
    * @return a list of node values
    */
  def inord(t: Tree): List[Int] = {
    def in(tr: Tree, acc: List[Int]): List[Int] = tr match {
      case Leaf() => acc
      case Node(v, l, r) => in(l, v :: in(r, acc))
    }

    in(t, Nil)
  }

  /**
    * Swaps left and right children of a Node if the Node is in a level divisible
    * by the given number
    * @param t: the tree to be swapped
    * @param layer: an integer which determines which layers of the Tree are to
    *             be swapped
    * @return the swapped Tree
    */
  def swap(t: Tree, layer: Int): Tree = {
    def sw(tr: Tree, d: Int): Tree = tr match {
      case Leaf() => Leaf()
      case Node(v, l, r) => if (d % layer != 0) Node(v, sw(l, d + 1), sw(r, d + 1))
      else  Node(v, sw(r, d + 1), sw(l, d + 1))
    }

    sw(t, 1)
  }
}