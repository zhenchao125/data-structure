package com.atguigu.datastructure.avl

/**
  * Author atguigu
  * Date 2019/12/23 17:21
  */
object AVLTreeDemo {
    def main(args: Array[String]): Unit = {
        //        val arr = Array(4, 3, 6, 5, 7) // (右右)需要左旋
        //        val arr = Array(10, 4, 16, 1, 20, 7, 8, 6, 9) //  需要左旋
        //        val arr = Array(10, 12, 8, 9, 7, 6) // (左左)需要右旋
        //        val arr = Array(30, 20, 40, 25, 50, 13, 15, 10, 8) // 需要右旋
        //        val arr: Array[Int] = Array(10, 11, 7, 6, 8, 9) // (左右) 先左旋再右旋
        val arr: Array[Int] = Array(10, 8, 16, 18, 14, 12) // (右左) 先右旋再左旋
        
        val tree: AVLTree[Int] = initAVLTree(arr)
        //        tree.add(8)
        println(tree.height())
        
        tree.infixForeach(x => print(x + "->"))
        println()
        
    }
    
    
    def initAVLTree(arr: Array[Int]): AVLTree[Int] = {
        
        val tree: AVLTree[Int] = new AVLTree[Int]
        arr.foreach(tree.add)
        tree
    }
    
}


class AVLTree[T: Ordering] {
    var root: AVLNode[T] = _
    
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
        if (root == null) root = new AVLNode[T](value)
        else {
            root.add(value)
            while (root.p != null) {
                root = root.p
            }
        }
        
        true
    }
    
    /**
      * 搜索元素
      *
      * @param value
      * @return
      */
    def search(value: T): AVLNode[T] = {
        if (root == null) null
        else {
            root.search(value)
        }
    }
    
    def height() = {
        if (root == null) -1
        else root.height()
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


class AVLNode[T: Ordering](var value: T) {
    
    // 左节点
    var left: AVLNode[T] = _
    // 右节点
    var right: AVLNode[T] = _
    // 父节点
    var p: AVLNode[T] = _
    
    /**
      * 删除最小节点
      *
      * @return
      */
    def deleteMin(): T = {
        var minNode: AVLNode[T] = this
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
    def search(v: T): AVLNode[T] = {
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
      * 对数进行平衡
      */
    def rotate(): Unit = {
        // 右右->左旋
        if (leftHeight() - rightHeight() < -1 && right.leftHeight() - right.rightHeight() < 0) {
            println("右右")
            leftRotate()
            return
        }
        
        // 左左 ->右旋
        if (leftHeight() - rightHeight() > 1 && left.leftHeight() - left.rightHeight() > 0) {
            println("左左")
            rightRotate()
            return
        }
        
        // 右左 -> 先右旋再左旋
        if (leftHeight() - rightHeight() < -1 && right.leftHeight() - right.rightHeight() > 0) {
            println("右左")
            right.rightRotate() // 右节点先右旋
            leftRotate() // 当前节点再左旋
            
            return
        }
        
        // 左右 -> 先左旋再右旋
        if (leftHeight() - rightHeight() > 1 && left.leftHeight() - left.rightHeight() < 0) {
            println("左右")
            left.leftRotate() // 右节点先右旋
            rightRotate() // 当前节点再左旋
            return
        }
        
    }
    
    def rightRotate(): Unit = {
        val tmpP: AVLNode[T] = p
        val tmpLeft: AVLNode[T] = left
        val tmpLeftRight: AVLNode[T] = left.right
        
        // 1. 让左节点的右节点成为当前节点的左节点
        left = tmpLeftRight
        // 2. 当前节点成为他的左节点的右节点
        tmpLeft.right = this
        
        // 3. 让左节点成为当前节点父节点的子节点(左/右)
        if (tmpP != null && tmpP.right == this) {
            tmpP.right = tmpLeft
        } else if (tmpP != null) {
            tmpP.left = tmpLeft
        }
        // 4. 更新父节点
        if (tmpLeftRight != null) tmpLeftRight.p = this
        this.p = tmpLeft
        tmpLeft.p = tmpP
    }
    
    def leftRotate(): Unit = {
        // 先暂存需要操作的节点  父节点, 右节点, 右的左节点
        val tmpP: AVLNode[T] = p
        val tmpRight: AVLNode[T] = right
        val tmpRightLeft: AVLNode[T] = right.left
        
        // 1. 让 右节点的左节点成为当前节点的右节点
        right = tmpRightLeft
        // 2. 当前节点成为他的右节点的左节点
        tmpRight.left = this
        // 3. 让右节点成为当前节点父节点的子节点(左/右)
        if (tmpP != null && tmpP.right == this) {
            tmpP.right = tmpRight
        } else if (tmpP != null) {
            tmpP.left = tmpRight
        }
        
        // 4. 更新父节点
        if (tmpRightLeft != null) tmpRightLeft.p = this
        this.p = tmpRight
        tmpRight.p = tmpP
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
                left = new AVLNode[T](v)
                left.p = this
            } else {
                left.add(v)
            }
        } else {
            if (right == null) {
                right = new AVLNode[T](v)
                right.p = this
            } else {
                right.add(v)
            }
        }
        
        println(s"旋转前: $value   " + (leftHeight() - rightHeight()))
        rotate()
        println(s"旋转后: $value   " + (leftHeight() - rightHeight()))
        true
    }
    
    /**
      *
      * 计算树的高度
      */
    def height(): Int = {
        val lh: Int = leftHeight()
        val rh: Int = rightHeight()
        lh.max(rh) + 1
    }
    
    def leftHeight(): Int = {
        if (left == null) -1
        else left.height()
    }
    
    def rightHeight(): Int = {
        if (right == null) -1
        else right.height()
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