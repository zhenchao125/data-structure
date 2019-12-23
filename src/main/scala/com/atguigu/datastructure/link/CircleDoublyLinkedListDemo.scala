package com.atguigu.datastructure.link

/**
  * Author atguigu
  * Date 2019/12/23 10:03
  */
object CircleDoublyLinkedListDemo {
    def main(args: Array[String]): Unit = {
        val linkedList = new CircleDoublyLinkedList[Int]
        linkedList.add(10)
        linkedList.add(20)
        linkedList.add(30)
        linkedList.add(15)
        
        println(linkedList.find(10))
        println(linkedList.find(1))
        linkedList.delete(10)
        linkedList.foreach(x => print(x + " -> "))
        linkedList.delete(15)
        println()
        linkedList.foreach(x => print(x + " -> "))
        println(linkedList.count)
        
    }
}


// 双向的循环的链表
class CircleDoublyLinkedList[T: Ordering] extends DoublyLinkedList[T] {
    override def add(value: T): Boolean = {
        // 父的添加仍然存在
        super.add(value)
        
        // 做成循环: head.pre = tail  tail.next=head
        head.pre = tail
        tail.next = head
        true
    }
    
    
    override def delete(value: T): Boolean = {
        
        if (super.delete(value)) {
            head.pre = tail
            tail.next = head
            true
        } else false
        
    }
    
    override def find(value: T): Option[Node] = {
        val ord: Ordering[T] = implicitly[Ordering[T]]
        if (head == null) return None
        else {
            var currentNode: Node = head
            while (currentNode != null) {
                if (ord.equiv(value, currentNode.value)) return Some(currentNode)
                // 如果已经找到了链表的尾部, 则不要继续寻找
                if (currentNode.eq(tail)) return None
                currentNode = currentNode.next
                
            }
        }
        None
    }
    
    override def foreach(op: T => Unit): Unit = {
        var currentNode: Node = head
        
        while (currentNode != null) {
            op(currentNode.value)
            // 尾部遍历过之后, 结束遍历
            if (currentNode.eq(tail)) return
            currentNode = currentNode.next
        }
    }
    
}
