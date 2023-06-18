object hw6 extends hwtest.hw("CS384"):
  import hwtest.binarytrees.{BinaryTree, Empty, Node}
  import scala.collection.mutable.{Queue}
  def userName = "emily"
    
  // DOCUMENTATION SIGNATURE BLOCK GOES HERE
  //I DID NOT USE ANY SOURCES OR ASSISTANCE REQUIRING DOCUMENTATION IN COMPLETING THIS ASSIGNMENT.

  // Visit the syllabus for more information:
  // https://eecscourses.westpoint.edu/courses/cs384/#documentation
  //modify the current list to include the first node and then every other node in the list
  
  
  /* enum BinaryTree[+A]:
    case Empty
    case Node(item: A, left: BinaryTree[A], right: BinaryTree[A]) */
  
  //pre_order
  def preOrderTrav(tree: BinaryTree[Int]): List[Int] = 
    var answer_list: List[Int] = List.empty
    def helper(t: BinaryTree[Int], list: List[Int]): List[Int] =
      tree match
        case Empty => list
        case Node(item, left, right) =>
          answer_list = item::answer_list
          helper(left, list)
          helper(right, list)
    helper(tree, answer_list)
    answer_list
  test("preOrderTrav", preOrderTrav, "tree")
  
  //in_order
  def inOrderTrav(tree: BinaryTree[Int]): List[Int] = 
    var answer_list: List[Int] = List.empty
    def helper(t: BinaryTree[Int], list: List[Int]): Unit = 
      tree match
        case Empty => return
        case Node(item, left, right) =>
          helper(left, list)
          answer_list = item::answer_list
          helper(right, list)
  //test("inOrderTrav", inOrderTrav, "tree")
  
  //post-order
//  def postOrderTrav(tree: BinaryTree[Int]): List[Int] = 
  //  var answer_list: List[Int] = List.empty
    //def helper(list: List[Int]): List[Int] =
      //if tree.left.isEmpty && tree.right.isEmpty then answer_list::tree.item
      //if tree.left.nonEmpty then helper(tree.left)
      //if tree.right.nonEmpty then helper(tree.right)
    //helper(answer_list)
    //answer_list
  //test("postOrderTrav", postOrderTrav, "tree")
  
  
  //level-order
  def levelOrderTrav(tree: BinaryTree[Int]): List[Int] = 
    var answer_list: List[Int] = List.empty
    def helper(tree: BinaryTree[Int]): List[Int] =
      if tree.left.isEmpty && tree.right.isEmpty then answer_list::tree.item
      if tree.left.nonEmpty && tree.Node == i then helper(tree.left)
      if tree.right.nonEmpty && tree.Node == i then helper(tree.right)
      else helper(tree.Node.item)
    helper(answer_list)
    answer_list
  test("levelOrderTrav", levelOrderTrav, "tree")
  
  
