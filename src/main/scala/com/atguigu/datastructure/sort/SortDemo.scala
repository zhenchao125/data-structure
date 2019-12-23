package com.atguigu.datastructure.sort

import scala.util.Random

/**
  * Author atguigu
  * Date 2019/12/23 8:10
  */
object SortDemo {
    def randomArr(num: Int) = {
        val arr = new Array[Int](num)
        for (i <- 0 until  num) {
            arr(i) = new Random().nextInt(100000000)
        }
        arr
    }
    def main(args: Array[String]): Unit = {
//        val arr1 = randomArr(100000)
        val arr1 = Array(40, 20, 30, -2)
//        swap(arr1, 0, 1)
        val start = System.currentTimeMillis()
        quickSort(arr1, 0, arr1.length - 1)
//        BubbleSort(arr1)
        println((System.currentTimeMillis() - start).toDouble / 1000)
        println(arr1.mkString(","))
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


