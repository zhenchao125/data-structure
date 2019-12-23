package com.atguigu.datastructure.hash

import com.atguigu.datastructure.link.DoublyLinkedList

import scala.reflect.ClassTag

/**
  * Author atguigu
  * Date 2019/12/23 15:00
  */
object HashDemo {
    def main(args: Array[String]): Unit = {
        val table = new HashTable[Int]
        table.add(10)
        table.add(20)
        table.add(10)
        table.add(30)
        table.add(31)
        table.printInfo
        
        
    }
}


class HashTable[T: ClassTag : Ordering] {
    val initLen = 2
    val arr: Array[DoublyLinkedList[T]] = new Array[DoublyLinkedList[T]](initLen)
    
    
    def add(value: T): Boolean = {
        val index: Int = value.hashCode().abs % initLen
        if (arr(index) == null) {
            arr(index) = new DoublyLinkedList[T]
        }
        arr(index).add(value)
        true
    }
    
    def printInfo = {
        for (link <- arr if link != null) {
            link.foreach(x => print(x + "->"))
            println()
        }
    }
    
}