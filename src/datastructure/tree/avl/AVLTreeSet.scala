package datastructure.tree.avl

object AVLTreeSet {
  
  def max(x:Int,y:Int) : Int = (x,y) match {
    case (x,y) if (x >= y) => x
    case _ => y
  }
  
  def apply[T<%Ordered[T]](args: T*): AVLTreeSet[T] = {
    var result: AVLTreeSet[T] = new EmptyAVLTreeSet[T]()
    if (args.length > 0) args.foreach { x => result = result+x  }
    result 
  }
}

sealed abstract class AVLTreeSet[T<%Ordered[T]](var h:Int) {
	def +(v:T):AVLTreeSet[T]
	def -(v:T):AVLTreeSet[T]
	def depth:Int
	def size: Int
	def isEmpty:Boolean
	def nonEmpty:Boolean
	def toList:List[T]  
	
	def contains(v: T) : Boolean = this match {
    case EmptyAVLTreeSet() => false
    case NonEmptyAVLTreeSet(x,sf,l,r) if (v == x)=> true
    case NonEmptyAVLTreeSet(x,sf,l,r) if (v < x) => l.contains(v)
    case NonEmptyAVLTreeSet(x,sf,l,r) if (v > x) => r.contains(v)    
  }

  protected def balance(t: AVLTreeSet[T]) : AVLTreeSet[T] = t match {
    case EmptyAVLTreeSet() => t
    case NonEmptyAVLTreeSet(x,sf,l,r) if (sf == 0) => t
    case NonEmptyAVLTreeSet(x,sf,l,r) if (sf == -1) => t
    case NonEmptyAVLTreeSet(x,sf,l,r) if (sf == 1)=> t
    case NonEmptyAVLTreeSet(x,sf1,NonEmptyAVLTreeSet(y,sf2,t1,t2),r) if ( (sf1 == -2) && (sf2 == -1) ) => t.rightRotation()
    case NonEmptyAVLTreeSet(x,sf1,l,NonEmptyAVLTreeSet(y,sf2,t1,t2)) if ( (sf1 == 2) && (sf2 == 1) ) => t.leftRotation()
    case NonEmptyAVLTreeSet(x,sf1,NonEmptyAVLTreeSet(y,sf2,t1,NonEmptyAVLTreeSet(z,_,t2,t3)),r) if ( (sf1 == -2) && (sf2 == 0) ) => t.rightDoubleRotation()
    case NonEmptyAVLTreeSet(x,sf1,l,NonEmptyAVLTreeSet(y,sf2,NonEmptyAVLTreeSet(z,_,t1,t2),t3)) if ( (sf1 == 2) && (sf2 == 0) ) => t.leftDoubleRotation()
  }
 	
  protected def rightRotation() : AVLTreeSet[T] = this match {
    case NonEmptyAVLTreeSet(x,_,NonEmptyAVLTreeSet(x2,_,l2,r2),r) => new NonEmptyAVLTreeSet[T](x2,1+AVLTreeSet.max(r2.h, r.h)-l2.h,l2,
        new NonEmptyAVLTreeSet(x,r.h-r2.h,r2,r))
    case _ => this
  }
   
  protected def leftRotation() : AVLTreeSet[T] = this match {
    case NonEmptyAVLTreeSet(x,_,l,NonEmptyAVLTreeSet(x2,_,l2,r2)) => new NonEmptyAVLTreeSet[T](x2,r2.h-(1+AVLTreeSet.max(l.h, l2.h)),
        new NonEmptyAVLTreeSet[T](x,l2.h-l.h,l,l2),r2)
    case _ => this
  }
  
  protected def rightDoubleRotation() : AVLTreeSet[T] = this match {
    case NonEmptyAVLTreeSet(x,_,l,r) => {val t1 = l.leftRotation();val t2 = new NonEmptyAVLTreeSet(x,r.h-t1.h,t1,r);t2.rightRotation()}
    case _ => this
  }

  protected def leftDoubleRotation() : AVLTreeSet[T] = this match {
    case NonEmptyAVLTreeSet(x,_,l,r) => {val t1 = r.rightRotation();val t2 = new NonEmptyAVLTreeSet(x,t1.h-l.h,l,t1);t2.leftRotation()}
    case _ => this
  }
	
}

private case class EmptyAVLTreeSet[T<%Ordered[T]]() extends AVLTreeSet[T](-1) {  
  override def toString = "["+h+"]"
  
	def +(v:T):AVLTreeSet[T] = new NonEmptyAVLTreeSet[T](v,0,new EmptyAVLTreeSet[T](),new EmptyAVLTreeSet[T]()) 	
	def -(v:T):AVLTreeSet[T] = this
	def depth:Int = 0
	def size: Int = 0
	def isEmpty: Boolean = true
	def nonEmpty:Boolean = false
	def toList: List[T] = List[T]()
}

private case class NonEmptyAVLTreeSet[T<%Ordered[T]](v:T,private var sf:Int,left:AVLTreeSet[T],right:AVLTreeSet[T]) extends AVLTreeSet[T](1+AVLTreeSet.max(left.h,right.h)) {   
  override def toString = "[" +v+","+sf+","+h+","+left+","+right+ "]"
  
	def depth:Int = 1+AVLTreeSet.max(left.h,right.h)
	def size:Int = 1+left.size+right.size
	def isEmpty:Boolean = false
	def nonEmpty:Boolean = true
	
	def toList: List[T] = this match {
    case NonEmptyAVLTreeSet(x,_,l,r) => l.toList:::x::r.toList
  }

  def +(v:T): AVLTreeSet[T] = this match {
    case NonEmptyAVLTreeSet(x,sf,l,r) if (v < x) => {val t = l+v; val t1= new NonEmptyAVLTreeSet(x,r.h-t.h,t,r); balance(t1);}
    case NonEmptyAVLTreeSet(x,sf,l,r) if (v > x) => {val t = r+v; val t1 = new NonEmptyAVLTreeSet(x,t.h-l.h,l,t); balance(t1)}
    case _ => this
  }
	
  def -(v: T): AVLTreeSet[T] = this match {
    case NonEmptyAVLTreeSet(x,_,l,r) if (v < x) => {val t=l-v; val t1 = new NonEmptyAVLTreeSet(x,r.h-t.h,t,r); balance(t1)}
    case NonEmptyAVLTreeSet(x,_,l,r) if (v > x) => {val t = r-v; val t1 = new NonEmptyAVLTreeSet(x,t.h-l.h,l,t);balance(t1)}
    case NonEmptyAVLTreeSet(x,_,EmptyAVLTreeSet(),EmptyAVLTreeSet()) if (x == v) => new EmptyAVLTreeSet()
    case NonEmptyAVLTreeSet(x,_,EmptyAVLTreeSet(),r) if (x == v) => r
    case NonEmptyAVLTreeSet(x,_,l,EmptyAVLTreeSet()) if (x == v) => l
    case NonEmptyAVLTreeSet(x,_,l,r) if (x == v) => {var y = inorderPredecesor(l); val t=l-y; val t1=new NonEmptyAVLTreeSet(y,r.h-t.h,t,r);balance(t1)}
  }
  
  protected def inorderPredecesor(t:AVLTreeSet[T]) : T = t match {
    case EmptyAVLTreeSet() => throw new AVLTreeSetException("No inorder predecesor in empty tree")    
    case NonEmptyAVLTreeSet(x,_,EmptyAVLTreeSet(),EmptyAVLTreeSet()) => x
    case NonEmptyAVLTreeSet(x,_,l,EmptyAVLTreeSet()) => x
    case NonEmptyAVLTreeSet(x,_,l,r) => inorderPredecesor(r)
  } 
}

case class AVLTreeSetException(msg:String)  extends Exception


