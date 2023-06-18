class DynamicIntArray():

  // `private` means a field or method is not accessible outside the class
  // https://docs.scala-lang.org/tour/classes.html#private-members-and-gettersetter-syntax
  private var capacity: Int = 10
  private var array: Array[Int] = new Array(capacity)
  private var size = 0

  // `override` means a method already exists but we want to re-write it 
  // inherited from Java has ()
  override def toString(): String =
    array.take(size).mkString(s"DIA.$capacity(", ", ", "")

  // Throws an error if the index given is out of bounds
  private def checkBounds(index: Int): Unit = 
    if index < 0 || index >= size then
      throw IndexOutOfBoundsException(s"Index $index out of bounds")

  // .get Returns the object at the given index
  def get(index: Int): Int = 
    checkBounds(index)
    array(index)
  
  // .apply() is a special function in Scala.
  def apply(index: Int): Int = 
    get(index)
  
  // .set Changes the value at the given index to the given item
  def set(index: Int, item: Int): Unit = 
    checkBounds(index)
    array(index) = item
  
  // .update() is a special function in Scala.
  def update(index: Int, item: Int): Unit =
    set(index, item)
  
  //.isEmpty returns true if the array is empty and false otherwise NOT COMPLETED
  def isEmpty: Boolean = size == 0
    
  // Checks to see if the array is full. If so, doubles the capacity. This
  // requires copying all existing elements to a new array.
  private def expand(): Unit = 
    if size == capacity then
      val array2 = new Array[Int](capacity*2)
      for i <- 0 until array.size do
        array(i) = array2(i)
      array = array2
        
      
  // .append adds a new element to the end of the array.
  def append(item: Int): Unit = 
    expand()
    array.update(size, item)
    size += 1
  
  // .removeLast removes the last element from the array and returns the removed item
  def removeLast(): Int = 
    size -= 1
    val element = array(size)
    set(size, ' ')
    element

  // .insert inserts a value at the given index. This requires copying each existing
  // element after the given index to the next higher place in the array.
  def insert(index: Int, item: Int): Unit = 
    expand()
    val array2 = new Array[Int](capacity)
    for i <- 0 until index do
      array2(i) = array(i)
    array2(index) = item
    for i <- index+1 until size do
      array2(i) = array(i)
    array = array2
    
  
  // .remove deletes the value at the given index. This requires copying each existing
  // element after the given index to the next lower place in the array.
  def remove(index: Int): Unit = 
    expand()
    val array2 = new Array[Int](capacity)
    for i <- 0 until index do
      array2.update(i, array(i))
    for i <- index+1 until size do
      array2(i-1) = array(i)
    array = array2
  
  // .slice selects an interval of elements from and including start until and excluding end
  def slice(start: Int, end: Int): DynamicIntArray = 
    val array2 = new DynamicIntArray
    for i <- start until end do
      array2(i) = array(i)
    array2
  
  //used for testing purposes not intended for general purpose use.
  def expose(): (Array[Int], Int, Int) = (array, capacity, size)
 
//Companion Object (The original DynamicIntArray that you will manipulate for others)
object DynamicIntArray:

  def empty: DynamicIntArray = new DynamicIntArray
    
  def apply(items: Int*): DynamicIntArray = 
    val array2 = new DynamicIntArray
    for i <- 0 until items.length do
      array2.set(i, items(i))
    array2
      
  
object hw3 extends hwtest.hw("CS384"):

  def userName = "emily"
  
  test("get", (i: Int, d: DynamicIntArray) => {d.get(i)}, "item", "dArray")
    
  test("apply", (i: Int,d: DynamicIntArray) => {d.apply(i)}, "item", "dArray")
  
  test("set", (i: Int, e: Int, d: DynamicIntArray) => {d.set(i,e); d}, "index", "item", "dArray")
  
  test("update", (i: Int,e: Int,d: DynamicIntArray) => {d.update(i,e); d}, "index", "item", "dArray")
  
  test("isEmpty", (d: DynamicIntArray) => {d.isEmpty}, "dArray")
  
  test("append", (i: Int,d: DynamicIntArray) => {d.append(i); d}, "item", "dArray")
  
  test("removeLast", (d: DynamicIntArray) => {d.removeLast()}, "dArray")
  
  test("insert", (i: Int,e: Int, d: DynamicIntArray) => {d.insert(i,e); d}, "index", "item", "dArray")
  
  test("remove", (i: Int ,d: DynamicIntArray) => {d.remove(i); d}, "index", "dArray")
  
  test("slice", (s: Int, e: Int ,d: DynamicIntArray) => {d.slice(s,e)}, "start", "end", "dArray")

// DO NOT TOUCH THE CODE BELOW 

import hwtest.{Testable, Src}
import hwtest.parsers.*

  given TestableDynamicIntArray: hwtest.Testable[DynamicIntArray] with
    val name = "DynamicIntArray"
    
    def parse: Src => DynamicIntArray = src => DynamicIntArray(hwtest.parsers.pList[Int](pInt)(src):_*)
    override def equiv(da1: DynamicIntArray, da2: DynamicIntArray): Boolean =
      val (a1,c1,s1) = da1.expose()
      val (a2,c2,s2) = da2.expose()
      s1 == s2 && a1.slice(0,s1).toList == a2.slice(0,s2).toList
    override def _show(da: DynamicIntArray): String =
      val (array, capacity, size) = da.expose()
      array.mkString("DynamicIntArray(",", ", s") [with capacity $capacity]" ) 
  