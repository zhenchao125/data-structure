package com.atguigu.datastructure.sort

import scala.util.Random

/**
  * Author atguigu
  * Date 2019/12/23 8:10
  */
object SortDemo {
    def randomArr(num: Int) = {
        val arr = new Array[Int](num)
        for (i <- 0 until num) {
            arr(i) = new Random().nextInt(100000000)
        }
        arr
    }
    
    def main(args: Array[String]): Unit = {
//                val arr1 = randomArr(100000)
//        val arr1 = Array(40, 20, 30, -2, 100, 20, Int.MaxValue,Int.MinValue)
        val arr1 = Array(10,20,90,3,55,69,23,20)
        //        swap(arr1, 0, 1)
        val start = System.currentTimeMillis()
        //        quickSort(arr1, 0, arr1.length - 1)
        mergeSort(arr1, 0, arr1.length - 1)
        //        BubbleSort(arr1)
        println((System.currentTimeMillis() - start).toDouble / 1000)
        println(arr1.mkString(","))
        
//        println(quick(arr1.toList).mkString(", "))
        
    }
    
    
    // 归并排序
    
    def mergeSort(arr: Array[Int], start: Int, end: Int): Unit = {
        if (start >= end) return
        val mid: Int = (start + end) / 2
        
        mergeSort(arr, start, mid) // 把数组拆成两部分  左
        mergeSort(arr, mid +1, end) // 右
        
        merge1(arr, start, mid, end)
    }
    
    // 增加哨兵的合并
    def merge1(arr: Array[Int], start: Int, mid: Int, end: Int): Unit = {
        // 增加一个最大值左为"哨兵"
        val left: Array[Int] = arr.slice(start, mid + 1 ) :+ Int.MaxValue
        val right: Array[Int] = arr.slice(mid + 1, end + 1) :+ Int.MaxValue
        
        var m = 0 // left的索引
        var n = 0 // right的索引
        for (i <- start to end) {
            if (left(m) < right(n)) {
                arr(i) = left(m)
                m += 1
            } else {
                arr(i) = right(n)
                n += 1
            }
        }
    }
    // 原来的合并
    def merge(arr: Array[Int], start: Int, mid: Int, end: Int): Unit = {
        val left: Array[Int] = arr.slice(start, mid + 1)
        val right: Array[Int] = arr.slice(mid + 1, end + 1)
        
        var m = 0 // left的索引
        var n = 0 // right的索引
        for (i <- start to end) {
            if (m > left.length - 1) {
                arr(i) = right(n)
                n += 1
            } else if (n > right.length - 1) {
                arr(i) = left(m)
                m += 1
                
            } else if (left(m) <= right(n)) {
                arr(i) = left(m)
                m += 1
            } else if (left(m) > right(n)) {
                arr(i) = right(n)
                n += 1
            }
        }
    }
    
    
    /*// 返回一个有序的数组
    def quick(arr: Array[Int]):Array[Int] = {
        arr match {
            case Array(a, rest@_*) =>
                (quick(rest.filter( _ <= a).toArray) :+ a) ++  quick(rest.filter( _ > a).toArray)
            case _ => Array[Int]()
        }
    }*/
    
    def quick(list: List[Int]): List[Int] = {
        list match {
            case Nil => Nil
            case ::(head, tail) =>
                quick(tail.filter(_ <= head)) ::: head :: quick(tail.filter(_ > head))
        }
    }
    
    
    // 对数组原地排序
    def quickSort(arr: Array[Int], left: Int, right: Int): Unit = {
        
        // 如果右指针已经小于等于左指针, 则结束排序
        if (left >= right) return
        
        val mid = quickPartition(arr, left, right)
        // 对左数组排序
        quickSort(arr, left, mid - 1)
        // 对右数组排序
        quickSort(arr, mid + 1, right)
        
    }
    
    /**
      * 快速排序的分区
      * Array(30, 50, 70, 60, 10, 20, -1, -100, 100, 20, 40, 22, 11, 51)
      *
      * @param arr
      * @param left
      * @param right
      * @return
      */
    def quickPartition(arr: Array[Int], left: Int, right: Int): Int = {
        var low: Int = left
        var high: Int = right
        val p: Int = arr(left)
        
        while (low < high) {
            // 找到一个大于 p的
            while (low <= high && arr(low) <= p) {
                low += 1
            }
            // low = 1
            // 找到一个小于等于p
            while (high >= low && arr(high) > p) {
                high -= 1
            }
            // high = 1
            
            // 交换元素
            if (low < high) {
                swap(arr, low, high)
            }
            
        }
        // 让第一个元素去到正确的位置
        swap(arr, left, high)
        high
    }
    
    
    def swap(arr: Array[Int], a: Int, b: Int) = {
        val tmp = arr(a)
        arr(a) = arr(b)
        arr(b) = tmp
    }
    
    
    def BubbleSort(arr: Array[Int]): Unit = {
        for (i <- 0 until arr.length - 1) { // 外层循环表示有多少元素(len - 1)需要排序
            for (j <- 0 until arr.length - 1 - i) { // 每个元素需要比较的次数(第 1 个比较 len-1, 第 2 个比较 len-1-1, ...)
                if (arr(j) > arr(j + 1)) { // 如果前面的大于后面的则交互二者的位置
                    swap(arr, j, j + 1)
                }
            }
        }
        
    }
    
    
}


