package linkedList

class SinglyLinkedList[A] extends ListADT[A] {

  private class Node(var data: A, var next: Node)
  private var head: Node = null
  var numElems = 0

  def apply(index: Int): A = {
    require(index >= 0)
    var rover = head
    for (i <- 0 until index) rover = rover.next
    rover.data
  }

  def update(index: Int, data: A): Unit = {
    require(index >= 0)
    var rover = head
    for (i <- 0 until index) rover = rover.next
    rover.data = data
  }

  def insert(index: Int, data: A): Unit = {
    require(index >= 0)
    if (index == 0) {
      head = new Node(data, head)
    } else {
      var rover = head
      for (i <- 0 until index - 1) rover = rover.next
      rover.next = new Node(data, rover.next)
      numElems += 1
    }
  }

  def remove(index: Int): A = {
    require(index >= 0)
    if (index == 0) {
      val ret = head.data
      head = head.next
      numElems -= 1
      ret
    } else {
      var rover = head
      for (i <- 0 until index - 1) rover = rover.next
      val ret = rover.next.data
      rover.next = rover.next.next
      numElems -=1
      ret
    }
  }

  def lengthOfSinglyLinkedList(): Int = numElems

  def display(): Unit = {
    var rover = head
    for (i <- 0 to numElems) {
      println(rover.data)
      rover = rover.next
    }
  }
  
  def reverse() = {
    var previous: Node = null
    var current: Node = head
    var next: Node = null
    while(current != null){
      next = current.next
      current.next = previous
      previous = current
      current = next
    }
    head = previous
    println("\nreversing list \n")
    for(i <- 0 to numElems){
      println(head.data)
      head = head.next
    }
  }
}