//CDT Neumann provided assistance on the following functions: siftUp, swap, the math function for parent, and deleteMin. He also provided assistance on the 
//implementation for the companion project.
//MY DOCUMENTATION IDENTIFIES ALL SOURCES USED AND ASSISTANCE RECEIVED IN COMPLETING THIS ASSIGNMENT.

class PriQueue():
  
  import scala.collection.mutable.ArrayBuffer

  private val buffer: ArrayBuffer[Int] = ArrayBuffer[Int]()
  
  override def toString(): String =
    buffer.mkString("("," ", ")")

  private def siftUp(index: Int): Unit = 
    if index > 0 then 
      if buffer(index) < buffer(parent(index)) then
        swap(parent(index), index)
        siftUp(parent(index))

  private def swap(idx1:Int, idx2:Int): Unit = 
    val temp1 = buffer(idx1)
    buffer(idx1) = buffer(idx2)
    buffer(idx2) = temp1

  private def siftDown(index: Int): Unit = 
    if left(index) < size then
      if buffer(left(index)) < buffer(index) then
        swap(index, left(index))
        siftDown(left(index))
        return
    if right(index) < size then
      if buffer(right(index)) < buffer(index) then
        swap(index, right(index))
        siftDown(right(index))

  private def parent(index: Int): Int = 
    (index-1)/2 

  private def left(index: Int): Int = 
    index*2 + 1

  private def right(index: Int): Int =
    index*2+2
    
  def isEmpty: Boolean = 
    buffer.isEmpty

  def min: Int = 
    buffer(0)

  def size: Int = 
    buffer.size

  def deleteMin: Int =
    if buffer.isEmpty then throw new NoSuchElementException ("trying to deleteMin from an empty heap")
    val temp = buffer(0)
    swap(0, size-1)
    buffer.remove(size-1)
    siftDown(0)
    temp
    

  def add(item: Int): Unit = 
    buffer.append(item)
    siftUp(size-1)
    
    
  //used for testing purposes not intended for general purpose use.
  def expose(): (ArrayBuffer[Int]) = (buffer)

//Companion Object (The original PriQueue that you will manipulate for others)
object PriQueue:
  def empty: PriQueue =
    val pq = new PriQueue
    pq

  def apply(elems: Int*): PriQueue = 
    val pqe = new PriQueue
    for item <- elems do
      pqe.add(item)
    pqe

object hw8 extends hwtest.hw("CS384"):

  def userName = "emily"
  
  test("isEmpty", (d: PriQueue) => {d.isEmpty}, "dArrayBuffer")
    
  test("min", (d: PriQueue) => {d.min}, "dArrayBuffer")
  
  test("size", (d: PriQueue) => {d.size}, "dArrayBuffer")
  
  test("add", (i: Int, d: PriQueue) => {d.add(i); d}, "item", "dArrayBuffer")
  //test("add", add, "item", "dArrayBuffer")
  
  test("deleteMin", (d: PriQueue) => {d.deleteMin; d}, "dArrayBuffer")

// DO NOT TOUCH THE CODE BELOW 

import hwtest.{Testable, Src}
import hwtest.parsers.*

  given TestablePriQueue: hwtest.Testable[PriQueue] with
    val name = "PriQueue"
    
    def parse: Src => PriQueue = src => PriQueue(hwtest.parsers.pList[Int](pInt)(src):_*)
    override def equiv(da1: PriQueue, da2: PriQueue): Boolean =
      val (a1) = da1.expose()
      val (a2) = da2.expose()
      //a1.slice(0,a1.size).toList == a2.slice(0,a2.size).toList
      a1.toList == a2.toList
    override def _show(da: PriQueue): String =
      val (buffer) = da.expose()
      buffer.mkString("PriQueue(",", ", s")")