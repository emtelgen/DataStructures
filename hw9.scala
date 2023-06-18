object hw9 extends hwtest.hw("CS384"):
  import hwtest.parsers.*
  import hwtest.{Testable,Src}

  def userName = "emily"
  //MY DOCUMENTATION IDENTIFIES ALL SOURCES USED AND ASSISTANCE RECEIVED IN COMPLETING THIS ASSIGNMENT.
  //Cadet Sebastian Nuemann helped me understand a little of the recursion for the insert part and the case Node for delete Min
  enum MinHeap:
    case Empty
    case Node(item: String, size: Int, left: MinHeap, right: MinHeap)
    
  //export MinHeap.*
  import MinHeap.{Empty, Node}
  // Represent the heap as a binary tree that satisfies the heap-order invariant:
  //   - for every non-null node t,
  //       t.item <= t.left.item (assuming left is not null)
  //       t.item <= t.right.item (assuming right is not null)
  // In addition, every node is annotated with its size (the number of
  // nodes in its subtrees plus itself).
  // HOT = Heap-Ordered Tree
  def isEmpty(h: MinHeap): Boolean = 
    h match
      case Empty => true
      case Node(item: String, size: Int, left: MinHeap, right: MinHeap) => false
  test("isEmpty", isEmpty, "h")
  
  def size(h: MinHeap): Int = 
    h match
      case Empty => 0
      case Node(item: String, size: Int, left: MinHeap, right: MinHeap) => size
  test("size", size, "h")
  
  def min(h: MinHeap): String = 
    h match
      case Empty => ""
      case Node(item: String, size: Int, left: MinHeap, right: MinHeap) => item
  test("min", min, "h")

  def insert(elem: String, h: MinHeap): MinHeap = 
    h match
      case Empty => new Node(elem, 1, Empty, Empty)
      case Node(item: String, size: Int, left: MinHeap, right: MinHeap) => 
        if elem < item then
          insert(elem, left)
        else
          insert(elem, right)
        new Node(elem, size+1, left, right)
  test("insert", insert, "elem", "h")

  def merge(h1: MinHeap, h2: MinHeap): MinHeap = 
    def determine_smaller(): MinHeap = 
      h1 match
        case Empty => h2
        case Node(item: String, size: Int, left: MinHeap, right: MinHeap) => 
          h2 match
            case Empty => h1
            case Node(item2: String, size2: Int, left2: MinHeap, right2: MinHeap) =>
              if item2 < item then h2
              else h1
    def size_smaller(left: MinHeap, right: MinHeap): Array[Int] = 
      var left_size = 0
      var right_size = 0
      left match
        case Empty => left_size = 0
        case Node(item: String, size: Int, left: MinHeap, right: MinHeap) =>
          left_size = 0
      right match
        case Empty => right_size = 0
        case Node(item: String, size: Int, left: MinHeap, right: MinHeap) =>
          right_size = 0
      var new_array: Array[Int] = Array(left_size, right_size)
      new_array
    val primary = determine_smaller()
    var alternate = h2
    if primary == h1 then alternate = h2
    else alternate = h1
    alternate match
      case Empty => primary
      case Node(item: String, size: Int, left: MinHeap, right: MinHeap) =>
        val size_list = size_smaller(left, right)
        if size_list(0) < size_list(1) then
          merge(primary, left)
        else 
          merge(primary, right)
  test("merge", merge, "h1", "h2")
  
  def deleteMin(h: MinHeap): MinHeap = h match
    case Empty => throw new NoSuchElementException("deleteMin of empty heap")
    case Node(item: String, size: Int, left: MinHeap, right: MinHeap) => merge(left, right)
  test("deleteMin", deleteMin, "h")

  // DO NOT TOUCH THE CODE BELOW THIS LINE --------------------------
  given TestableMinHeap: Testable[MinHeap] =
    new Testable[MinHeap]:
      val name = s"MinHeap"
      def parse: Src => MinHeap =
        choose(
          'E' -> const(Empty),
          //'L' -> chain(TA.parse, Node(_,Empty,Empty)),
          'T' -> chain(pString, pInt, parse, parse, Node(_,_,_,_))
        )
      override def equiv(x: MinHeap, y: MinHeap): Boolean =
        (x,y) match
          case (Empty, Empty) => true
          case (Node(xitem,xsize,xleft,xright), Node(yitem,ysize,yleft,yright)) =>
            xitem == yitem && xsize == ysize && equiv(xleft, yleft) && equiv(xright,yright)
          case (_, _) => false
    
  def encode(mh: MinHeap): String = mh match
    case Empty => "E"
    case Node(item, size, left, right) => s"T\"$item\" $size ${encode(left)} ${encode(right)}"