import org.scalatest.funsuite.AnyFunSuite

class LinkedListTestCase extends AnyFunSuite {

  test("InsertNode and TraverseNode in LinkedList") {
    val list = new LinkedList[Int]
    list.insertNode(1)
    list.insertNode(2)
    list.insertNode(3)
    list.insertNode(4)
    assert(list.head.data == 1)
    assert(list.head.next.data == 2)
    assert(list.head.next.next.data == 3)
    assert(list.head.next.next.next.data == 4)

    val linkedList = scala.collection.mutable.ListBuffer.empty[Int]
    list.traverse { data => linkedList += data }
    assert(linkedList == Seq(1, 2, 3, 4))
  }
  test("SearchNode  element in LinkedList") {
    val list = new LinkedList[String]
    list.insertNode("America")
    list.insertNode("India")
    list.insertNode("Europe")
    list.insertNode("China")

    val node = list.search("Europe")
    assert(node.isDefined)
    assert(node.get.data == "Europe")
  }

  test("DeleteNode element from LinkedList") {
    val list = new LinkedList[Int]
    list.insertNode(101)
    list.insertNode(201)
    list.insertNode(301)
    list.deleteNode(201)
    assert(list.head.data == 101)
    assert(list.head.next.data == 301)

    val linkedList = scala.collection.mutable.ListBuffer.empty[Int]
    list.traverse { data => linkedList += data }
    assert(linkedList == Seq(101, 301))
  }
}