package com.atguigu.datastructure.tree

/**
  * Author atguigu
  * Date 2019/12/23 16:13
  */
object BinaryTreeDemo {
    def main(args: Array[String]): Unit = {
        
        val tree: BinaryTree[Int] = initTree()
        
        //        tree.preForeach(println)
//        tree.infixForeach(println)
        tree.postForeach(println)
    }
    
    def initTree() = {
        val tree = new BinaryTree[Int]
        tree.root = new TreeNode[Int](10)
        tree.root.left = new TreeNode[Int](9)
        tree.root.right = new TreeNode[Int](20)
        tree.root.right.left = new TreeNode[Int](15)
        tree.root.right.right = new TreeNode[Int](35)
        tree
        
        
    }
}


class BinaryTree[T] {
    var root: TreeNode[T] = _
    
    /**
      * 先序遍历:
      * 根->左->右
      * 中序遍历:
      * 左->根-> 右
      * 后序遍历:
      * 左->右->根
      *
      * @param op
      */
    def preForeach(op: T => Unit): Unit = {
        if (root == null) return
        else {
            op(root.value)
            if (root.left != null) root.left.preForeach(op)
            if (root.right != null) root.right.preForeach(op)
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
    
    def postForeach(op: T => Unit): Unit = {
        if (root != null) {
            if (root.left != null) root.left.postForeach(op)
            if (root.right != null) root.right.postForeach(op)
            op(root.value)
        }
    }
    
    
}

class TreeNode[T](var value: T) {
    
    
    // 左节点(左子树)
    var left: TreeNode[T] = _
    // 右节点
    var right: TreeNode[T] = _
    // 父节点(双亲节点)
    var p: TreeNode[T] = _
    
    
    def preForeach(op: T => Unit): Unit = {
        op(value)
        if (left != null) left.preForeach(op)
        if (right != null) right.preForeach(op)
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
    
    /**
      * 后序遍历
      *
      * @param op
      * @return
      */
    def postForeach(op: T => Unit): Unit = {
        if (left != null) left.postForeach(op)
        if (right != null) right.postForeach(op)
        op(value)
    }
}