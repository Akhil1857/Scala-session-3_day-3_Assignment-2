/*Node is the node of the linked list which contains the data and the address to the next node
 through which they are interlinked where [A] is the Generic ParameterType it can be Int,String*/
class Node[A](val data: A, var next: Node[A])

class LinkedList[A] {
  var head: Node[A] = null

  //insertNode inserts the elements at the ends of the list
  def insertNode(data: A): Unit = {

    // It helps to points to the next node
    def pointerToNextNode(node: Node[A]) {
      if (node.next == null)
        node.next = new Node[A](data, null)
      else
        pointerToNextNode(node.next)
    }

    if (head == null)
      head = new Node(data, null)
    else
      pointerToNextNode(head)
  }

  //deleteNode delete the node at which the data is being present
  def deleteNode(data: A): Unit = {
    def pointerToNextNode(node: Node[A]): Unit = {
      if (node == null || node.next == null) return
      if (node.next.data == data)
        node.next = node.next.next
      else
        pointerToNextNode(node.next)
    }

    if (head != null && head.data == data)
      head = head.next
    else
      pointerToNextNode(head)
  }

  //Traverse is to traverse or reach to the every element of the linked list
  def traverse(f: A => Unit): Unit = {
    def pointerToNextNode(node: Node[A]): Unit = {
      if (node != null) {
        f(node.data)
        pointerToNextNode(node.next)
      }
    }

    pointerToNextNode(head)
  }

  //Search is used for searching the node int he linked list
  def search(data: A): Option[Node[A]] = {
    def pointerToNextNode(node: Node[A]): Option[Node[A]] = {
      if (node == null)
        None
      else if (node.data == data)
        Some(node)
      else
        pointerToNextNode(node.next)
    }

    pointerToNextNode(head)
  }
}

