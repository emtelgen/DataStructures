object hw1 extends hwtest.hw("CS384"):

  def userName = "emily"
  
  
  

  // DOCUMENTATION SIGNATURE BLOCK GOES HERE
  //I DID NOT USE ANY SOURCES OR ASSISTANCE REQUIRING DOCUMENTATION IN COMPLETING THIS ASSIGNMENT.
  // Visit the syllabus for more information:
  // https://eecscourses.westpoint.edu/courses/cs384/#documentation

  def numDigits(number: Int): Int =
    var amount = 0
    var number_var = number
    while number_var > 10 do
      number_var = number_var/10
      amount += 1
    amount
  test("numDigits", numDigits, "number")

  def firstOdd(array: Array[Int]): Option[Int] =
   for i <- array.length do
    if array(i) % 2 != 0 then
     Some(array(i))
   None

  test("firstOdd", firstOdd, "array")

  def maxChar(string: String): (Char,Int) =
    var items = Array.fill(string.length)((' ',0))
    var max = 0
    var max_char = ' '
    var counter = -1
    for item <- string do
      counter += 1
      if item in items then
        items(counter)(1) += 1
      else 
        for item <- items do
          if items(counter)(0) == ' ' then
            items(counter)(0) = item
            items(counter)(1) = 1
    counter = -1
    for element <- items do
      counter += 1
      if items(counter)(1) > max then
        max = items(counter)(1)
        max_char = items(counter)(0)
    val final_tuple = (max_char, max)
    final_tuple

  test("maxChar", maxChar, "string")