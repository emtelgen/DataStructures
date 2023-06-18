object ice:

  def prepend(list: List[Int], elem: Int): List[Int] = 
    elem :: list

  def headOfList(list: List[Int]): Int =
    list.headOption.getOrElse(-1)
  
  def size(list: List[Int]): Int = 
    if list.isEmpty then return 0
    val size = 1 + size(list.tail)
    
  val realist = prepend(List(1, 2, 3, 4), 99)
  realist.foreach(println(_))
  val testing = headOfList(List(1, 2, 3, 4))
  print(testing)
  val sizing = size(List(1, 2, 3))
  print(sizing)

  @main def run:Unit = {}
    
    
    