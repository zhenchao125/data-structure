package com.atguigu.datastructure.link

/**
  * Author atguigu
  * Date 2019/12/23 10:22
  */
object Josephu {
    def main(args: Array[String]): Unit = {
        println("\n" + start(70, 3, 3))
    }
    
    // 游戏刚开始
    def start(n: Int, k: Int, m: Int):Int = {
        val linkedList = new CircleDoublyLinkedList[Int]
        // 给链表中初始人
        for (num <- 1 to n) {
            linkedList.add(num)
        }
        
        var startNode: linkedList.Node = linkedList.find(k).get.pre
        
        while (linkedList.count > 1){
            for(i <- 1 to m){
                startNode = startNode.next
            }
            linkedList.delete(startNode.value)
            print(startNode.value + " -> ")
            startNode = startNode.pre
        }
        linkedList.head.value
    }
}
