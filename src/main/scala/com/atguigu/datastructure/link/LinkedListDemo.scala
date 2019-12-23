package com.atguigu.datastructure.link


object DoublyLinkedListDemo {
    def main(args: Array[String]): Unit = {
        val linkedList = new DoublyLinkedList[Int]
        linkedList.add(10)
        linkedList.add(20)
        linkedList.add(30)
        linkedList.add(15)
        
        linkedList.foreach(println)
        println("----")
        linkedList.delete(10)
        linkedList.foreach(println)
        println("----")
        linkedList.delete(22)
        linkedList.foreach(println)
        println("----")
        
        linkedList.add(100)
        linkedList.foreach(println)
        println("----")
    }
}

// 一定有一个隐式值  Ordering[T]
class DoublyLinkedList[T: Ordering] {
    var head: Node = _
    var tail: Node = _
    
    var count: Int = 0
    
    
    def add(value: T): Boolean = {
        
        val newNode: Node = Node(value, null, null)
        // 0. 先判断链表是否为空
        if (head == null) {
            head = newNode
            tail = newNode
        } else {
            // 1. 让tail的next指向新节点, 新节点的pre指向现在的tail
            tail.next = newNode
            newNode.pre = tail
            // 2. 更新tail指向新增加的节点
            tail = newNode
        }
        // 3. count + 1
        count += 1
        true
    }
    
    /**
      * 删除元素
      *
      * @param value
      * @return
      */
    def delete(value: T): Boolean = {
        find(value) match {
            case Some(node) =>
                if (node == head) {
                    head = node.next
                    head.pre = null
                } else if (node == tail) {
                    tail = node.pre
                    tail.next = null
                } else {
                    // . 扎到这个节点的上一个元素和下一个元素
                    val pre: Node = node.pre
                    val next: Node = node.next
                    pre.next = next
                    next.pre = pre
                }
                count -= 1
                true
            case None => false
        }
    }
    
    /**
      * 查找指定的元素是否存在
      *
      * @param value
      * @return
      */
    def find(value: T): Option[Node] = {
        val ord: Ordering[T] = implicitly[Ordering[T]]
        if (head == null) return None
        else {
            var currentNode: Node = head
            while (currentNode != null) {
                if (ord.equiv(value, currentNode.value)) return Some(currentNode)
                currentNode = currentNode.next
            }
        }
        None
    }
    
    /**
      * 遍历双向链表
      *
      * @param op
      */
    def foreach(op: T => Unit) {
        
        var currentNode: Node = head
        
        while (currentNode != null) {
            op(currentNode.value)
            currentNode = currentNode.next
        }
    }
    
    
    // 表示链表中具体的一个节点
    case class Node(value: T, var pre: Node, var next: Node) {
        override def toString: String = value.toString
    }
    
}

