object hw7 extends hwtest.hw("CS384"):
  import hwtest.searchtrees.{SearchTree, Empty, Node}
  import scala.collection.mutable.{HashSet}
  
  def userName = "emily"
    
  // DOCUMENTATION SIGNATURE BLOCK GOES HERE
  // Visit the syllabus for more information:
  //I DID NOT USE ANY SOURCES OR ASSISTANCE REQUIRING DOCUMENTATION IN COMPLETING THIS ASSIGNMENT.
  
  
  /* enum SearchTree[+A]:
    case Empty
    case Node(left: SearchTree[A], item: A, right: SearchTree[A]) */
  

  def search(bst: SearchTree[Int], target: Int): Boolean = 
    bst match
      case Empty => false
      case Node(left, item, right) => 
        if item == target then true
        else if target < item then search(left, target)
        else search(right, target)
        
  test("search", search, "bst", "target")
  
  def insert(bst: SearchTree[Int], newItem: Int): SearchTree[Int] = 
    bst match
      case Empty => Node(Empty, newItem, Empty)
      case Node(left, item, right) =>
        if newItem > item then Node(left, item, insert(right, newItem))
        else if newItem < item then Node(insert(left, newItem), item, right)
        else bst
     
  testV("insert", insert, "bst", "newItem"){(bst, newItem, insertResult) =>
    checkBst(insertResult)
    assert(toSet(insertResult) == toSet(bst) + newItem)
  } 

//  DO NOT EDIT THE TEST CODE BELOW_---------------------------------
  import org.scalatest.Assertions._
  def checkBst(bst: SearchTree[Int], min: Long = Long.MinValue, max: Long = Long.MaxValue): Unit =
    bst match
      case Empty => {}
      case Node(left, item, right) =>
        assert(min != item && item != max, "your tree has duplicate values")
        if item < min || item > max then fail("your tree is out of order at " + item)
        checkBst(left, min, item)
        checkBst(right, item, max)

  def toSet(bst: SearchTree[Int]): HashSet[Int] =
    val set = HashSet.empty[Int]
    def addItem(bst: SearchTree[Int], set: HashSet[Int]): HashSet[Int] =
      bst match
        case Node(left, item, right) =>
          set += item
          addItem(left, set)
          addItem(right, set)
        case Empty => {}
      set
    addItem(bst, set)
    
    


