import annotation.tailrec
object Problem2 {
  //def fib(i: BigInt, j: BigInt) = 
	//	Iterator.iterate((i,j)){ case (a,b) => (b,a+b) }.map(_._1)
  def fibFrom(a: BigInt, b: BigInt): Stream[BigInt] = a #:: fibFrom(b, 
a + b);
  def evenOf(x:BigInt) = x % 2 == 0;
  
  
  def main(args: Array[String]): Unit = {
    println( fibFrom(1,1).view takeWhile(_<= 4000000) filter evenOf sum);
  }
} 