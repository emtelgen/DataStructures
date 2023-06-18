object hw2 extends hwtest.hw("CS384"):
  
  def userName = "emily"
    
  // DOCUMENTATION SIGNATURE BLOCK GOES HERE
    //I DID NOT USE ANY SOURCES OR ASSISTANCE REQUIRING DOCUMENTATION IN COMPLETING THIS ASSIGNMENT.
  // Visit the syllabus for more information:
  // https://eecscourses.westpoint.edu/courses/cs384/#documentation
  
  def indexOfMinElement(array: Array[Int]): Int = 
    if array.isEmpty then return -1
    var minimum = array(0)
    var min_index = 0
    for i <- 0 until array.length do 
      if array(i) < minimum then
        minimum = array(i)
        min_index = i
    min_index
  test("indexOfMinElement", indexOfMinElement, "array")
  
  def reverseElementsOfArray(array: Array[Int]): Unit = 
    for i <- 0 until array.length/2 do
      val first_number = array(i)
      array(i) = array(array.length-i-1)
      array(array.length-i-1) = first_number
  test("reverseElementsOfArray", (a:Array[Int]) => {reverseElementsOfArray(a);a} , "array")
  
  def swapElements(array: Array[Int]): Unit = 
    for i <- 0 until array.length - 1 by 2 do 
      var temp = array(i)
      array(i) = array(i+1)
      array(i+1) = temp
  test("swapElements", (a:Array[Int])=>{swapElements(a);a}, "array")
  
  def mergeArrays(array1: Array[Int], array2: Array[Int]): Array[Int] = 
  //Making the new array 
    val answer = Array.fill[Int](array1.length+array2.length)((0))
    var i = 0
    var j = 0
    var k = 0
    while i != array1.length && j != array2.length do
      if array1(i) < array2(j) then
        answer(k) = array1(i)
        i += 1
      else
        answer(k) = array2(j)
        j += 1
      k += 1
    for i2 <- i until array1.length do
      answer(k) = array1(i2)
      k += 1
    for j2 <- j until array2.length do
      answer(k) = array2(j2)
      k += 1 
    answer
  test("mergeArrays",mergeArrays, "array1", "array2")