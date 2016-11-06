package datastructure.tree.avl

object AppAVLTreeSet extends App {
  val t1 = new EmptyAVLTreeSet[Int]();
  val t2 = t1+1+2+3+4+5+6+7+8+9+10
  println(t2)
  println(t2.toList)
  val t3 = t2-1-2-3-4-5-6-7-8
  println(t3)
  println(t3.toList)  
  println(t2.size)
  println(t3.size)
  val t4 = t2-10
  println(t4.toList)
  println(t4)
  println(t4.size)
  println(t4.contains(10))
  println(t4.contains(9))
}