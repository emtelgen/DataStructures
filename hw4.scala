object hw4 extends hwtest.hw("CS384"):
  
  def userName = "emily"
    
  // DOCUMENTATION SIGNATURE BLOCK GOES HERE
  //MY DOCUMENTATION IDENTIFIES ALL SOURCES USED AND ASSISTANCE RECEIVED IN COMPLETING THIS ASSIGNMENT.
  //Sebastian Neumann. 3 OCT 2022. Assistance given to author. CDT Neumann helped me learn the syntax to instantiate a new list for line 21. West Point, NY.
  // Visit the syllabus for more information:
  // https://eecscourses.westpoint.edu/courses/cs384/#documentation
  
  //Use Iteration / Loops
  def productI(list: List[Int]): Int = 
    var product = 1
    for number <- list do
      product = product*number
    product
  test("productI", productI, "list")
  
  //Use Iteration / Loops
  def reverseI(list: List[Int]): List[Int] = 
    var reversed_list: List[Int] = List.empty
    for element <- list do
      reversed_list = element::reversed_list
    reversed_list
  test("reverseI", reverseI, "list")
  
  //Use Iteration / Loops
  def takeI(list: List[Int], n: Int): List[Int] = 
    var answer_list: List[Int] = List.empty
    var i = 0
    for element <- list do
      if i < n then
        answer_list = element::answer_list
      i += 1
    reverseI(answer_list)
  test("takeI", takeI, "list", "n")
  
  //Use Iteration / Loops
  def addToEndI(list: List[Int], elem: Int): List[Int] = 
    var answer_list: List[Int] = List.empty
    for element <- list do
      answer_list = element::answer_list
    answer_list = elem::answer_list
    reverseI(answer_list)
  test("addToEndI", addToEndI, "list", "elem")
  
  //Use Iteration / Loops
  def appendI(list1: List[Int], list2: List[Int]): List[Int] = 
    var answer_list: List[Int] = List.empty
    for element <- list1 do
      answer_list = answer_list:+element
    for element <- list2 do
      answer_list = answer_list:+element
    answer_list
  test("appendI", appendI, "list1", "list2")
  
  // You must use recursion! You must submit a DCG diagram
  def productR(list: List[Int]): Int = 
    if list.isEmpty then 1
    else list.head * productR(list.tail)
  test("productR", productR, "list")
  
  //you must use recursion! You must submit a DCG diagram
  def reverseR(list: List[Int]): List[Int] = 
    def accumulate(reverseSoFar: List[Int], list: List[Int]): List[Int] =
      if list.isEmpty then reverseSoFar
      else accumulate(list.head::reverseSoFar, list.tail)
    var reversed_list: List[Int] = List.empty
    accumulate(reversed_list, list)
  test("reverseR", reverseR, "list")
  
  //you must use recursion! You must submit a DCG diagram  
  def takeR(list: List[Int], n: Int): List[Int] = 
    def accumulate(a: Int, takeSoFar: List[Int], list: List[Int]): List[Int] =
      if a==n then takeSoFar
      else accumulate(a+1, list.head::takeSoFar, list.tail)
    var new_list: List[Int] = List.empty
    reverseR(accumulate(0, new_list, list))
  test("takeR", takeR, "list", "n")
  
  //you must use recursion! You must submit a DCG diagram 
  def addToEndR(list: List[Int], elem: Int): List[Int] = 
    def accumulate(list: List[Int], elem: Int, addSoFar: List[Int]): List[Int] = 
       if list.isEmpty then elem::addSoFar
       else accumulate(list.tail, elem, list.head::addSoFar)
    var new_list: List[Int] = List.empty
    reverseR(accumulate(list, elem, new_list))
  test("addToEndR", addToEndR, "list", "elem")
  
  //you must use recursion! You must submit a DCG diagram 
  def appendR(list1: List[Int], list2: List[Int]): List[Int] = 
    def accumulate(list1: List[Int], listSoFar: List[Int]): List[Int] = 
      def accumulate2(list2: List[Int], listSoFar: List[Int]): List[Int] = 
        if list2.tail.isEmpty then list2.head::listSoFar
        else accumulate2(list2.tail, list2.head::listSoFar)
      if list1.isEmpty then accumulate2(list2, listSoFar)
      else accumulate(list1.tail, list1.head::listSoFar)
    var new_list: List[Int] = List.empty
    reverseR(accumulate(list1, new_list))
  test("appendR", appendR, "list1", "list2")