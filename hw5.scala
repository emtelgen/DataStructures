object hw5 extends hwtest.hw("CS384"):
  import hwtest.mlist.*
  
  def userName = "emily"
    
  // DOCUMENTATION SIGNATURE BLOCK GOES HERE
  // Visit the syllabus for more information:
  // https://eecscourses.westpoint.edu/courses/cs384/#documentation
 //I DID NOT USE ANY SOURCES OR ASSISTANCE REQUIRING DOCUMENTATION IN COMPLETING THIS ASSIGNMENT.

  //modify the current list to include the first node and then every other node in the list
  
  def everyOther(list: MList): Unit =
    var p1 = list.tail
    var p2 = list.tail
    if list.head == 0 || list.head == 1 then {}
    else
      while p2.nonEmpty && p2.tail.nonEmpty do
          p2 = p2.tail.tail
          p1.tail = p2
          p1 = p2
          list.head = list.head - 1
  test[MList, MList]("everyOther", list => {everyOther(list); list}, "list")

  def keepOdds(list: MList): Unit = 
    var p1 = list.tail
    var p2 = list.tail
    if list.head == 0 then {}
    else if list.head == 1 && list.tail.head % 2 == 0 then
      list.head = 0
      list.tail = list.tail.tail
    else
      while p2.nonEmpty do
        if p2.head % 2 == 0 then
          p2 = p2.tail
          p1.tail = p2
          p1 = p2
          list.head = list.head - 1
        else
          p2 = p2.tail
  test[MList, MList]("keepOdds", list => {keepOdds(list); list}, "list")

  def splice(hdr1: MList, index: Int, hdr2: MList): Unit = 
    var p1 = hdr1.tail
    if hdr1.head == 0 && hdr2.head == 0 then {}
    else
      for i <- 0 until index do
        p1 = p1.tail
      var p2 = p1.tail
      p1 = hdr2.tail
      for i <- 0 until hdr2.head-1 do
        p1 = p1.tail
      p1 = p2
      for i <- index until hdr1.head-1 do
        p1 = p1.tail
  test[MList, Int, MList, MList]("splice", (hdr1, index, hdr2) => {splice(hdr1, index, hdr2 ); hdr1 }, "hdr1", "index", "hdr2")
  
  def reverse(list: MList): Unit = 
    var p1 = list.tail
    var p2 = list.tail
    if list.head == 0 || list.head == 1 then {}
    else 
      while p2.tail.nonEmpty do
        p2 = p2.tail
      p1 = p2
      for i <- 0 until list.head do
        p1 = p2
        p2.tail = p1
  test[MList, MList]("reverse", list => {reverse(list); list}, "list")
  
  
  
  
  
  