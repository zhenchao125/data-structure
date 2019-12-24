package com.atguigu.datastructure.searchtree

/**
  * Author atguigu
  * Date 2019/12/23 17:21
  */
object SearchBinaryTreeDemo {
    def main(args: Array[String]): Unit = {
        val arr: Array[Int] = Array(1,2,3,4,5,6,7,8)
        val tree: SearchBinaryTree[Int] = initSearchTree(arr)
        tree.infixForeach(x => print(x + "->"))
        tree.delete(20)
        println()
        tree.infixForeach(x => print(x + "->"))
        
        //        println(tree.search(8))
        //        println(tree.search(12))
        //        println(tree.search(20))
    }
    
    def initSearchTree(arr: Array[Int]): SearchBinaryTree[Int] = {
        
        val tree: SearchBinaryTree[Int] = new SearchBinaryTree[Int]
        arr.foreach(tree.add)
        tree
    }
}


class SearchBinaryTree[T: Ordering] {
    var root: TreeNode[T] = _
    
    /**
      * 删除元素
      *
      * @param v
      */
    def delete(v: T): Boolean = {
        if (root == null) false
        else if (root.value == v) { // 如果删除的是根节点
            if (root.left == null && root.right == null) root = null
            else if (root.left != null && root.right != null) { // 左右子都不为空
                root.value = root.right.deleteMin()
            } else { // 左右子树有一个不为空, 让根节点指向这个这个不为空的节点
                root = if (root.left != null) root.left else root.right
            }
            true
        } else {
            // 删除他的子节点
            root.delete(v)
        }
    }
    
    /**
      * 给排序二叉树添加元素
      *
      * @param value
      * @return
      */
    def add(value: T): Boolean = {
        if (root == null) root = new TreeNode[T](value)
        else root.add(value)
        
        true
    }
    
    /**
      * 搜索元素
      *
      * @param value
      * @return
      */
    def search(value: T): TreeNode[T] = {
        if (root == null) null
        else {
            root.search(value)
        }
    }
    
    
    /**
      * 中序遍历
      *
      * @param op
      */
    def infixForeach(op: T => Unit): Unit = {
        if (root != null) {
            if (root.left != null) root.left.infixForeach(op)
            op(root.value)
            if (root.right != null) root.right.infixForeach(op)
        }
    }
}


class TreeNode[T: Ordering](var value: T) {
    
    // 左节点
    var left: TreeNode[T] = _
    // 右节点
    var right: TreeNode[T] = _
    // 父节点
    var p: TreeNode[T] = _
    
    /**
      * 删除最小节点
      *
      * @return
      */
    def deleteMin(): T = {
        var minNode: TreeNode[T] = this
        while (minNode.left != null) { // 循环结束找到最小节点
            minNode = minNode.left
        }
        // 用来删除节点
        minNode.delete(minNode.value)
        minNode.value
    }
    
    /**
      * 删除节点
      *
      * @param v
      * @return
      */
    def delete(v: T): Boolean = {
        val ord: Ordering[T] = implicitly[Ordering[T]]
        // 1.判断当前节点是否是要删除的节点
        if (ord.equiv(v, value)) {
            // 1. 先判断当前节点是他父节点的左节点还是右节点
            var isLeft: Boolean = true
            if (p != null && p.right != null && ord.equiv(p.right.value, value)) {
                isLeft = false
            }
            // 2. 当前节点是否为叶子节点
            if (left == null && right == null) {
                if (isLeft) p.left = null
                else p.right = null
            } else if (left != null && right != null) { // 左孩子和右孩子都在
                value = right.deleteMin()
            } else { // 有一个不为空
                // 找到非空的子节点
                val notNullNode = if (left != null) left else right
                notNullNode.p = p // 让非空的子节点的父节点, 指向当前的子节点的父节点
                if (isLeft) p.left = notNullNode
                else p.right = notNullNode
            }
            true
        } else if (ord.lt(v, value)) { // 要删的值小于当前值, 去左变删
            if (left == null) false
            else left.delete(v)
        } else {
            if (right == null) false
            else right.delete(v)
        }
        
    }
    
    /**
      * 查找节点
      *
      * @param value
      * @return
      */
    def search(v: T): TreeNode[T] = {
        println(v)
        val ord: Ordering[T] = implicitly[Ordering[T]]
        if (ord.equiv(v, value)) {
            this
        } else if (ord.lt(v, value)) {
            if (left == null) null
            else left.search(v)
        } else {
            if (right == null) null
            else right.search(v)
        }
    }
    
    
    /**
      * 给当前节点添加元素
      * val arr: Array[Int] = Array(8, 4, 9, 10, 1, 6, 7, 12)
      *
      * @param value
      * @return
      */
    def add(v: T): Boolean = {
        val ord: Ordering[T] = implicitly[Ordering[T]]
        
        // 如果值小于当前节点
        if (ord.lteq(v, value)) {
            if (left == null) { // 在左子树添加.
                left = new TreeNode[T](v)
                left.p = this
            } else {
                left.add(v)
            }
        } else {
            if (right == null) {
                right = new TreeNode[T](v)
                right.p = this
            } else {
                right.add(v)
            }
        }
        true
    }
    
    
    /**
      * 中序遍历
      *
      * @param op
      * @return
      */
    def infixForeach(op: T => Unit): Unit = {
        if (left != null) left.infixForeach(op)
        op(value)
        if (right != null) right.infixForeach(op)
        
    }
    
}