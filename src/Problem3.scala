object Problem3 {
  def largestPrimeFactor(b : BigInt) = {
   def loop(f:BigInt, n: BigInt): BigInt =
     if (f == n) n else 
     if (n % f == 0) loop(f, n / f) 
     else loop(f + 1, n)
  loop (BigInt(2), b)
 }
  
  def bigIterator(start: BigInt, end: BigInt, step: BigInt = 1) = 
		  Iterator.iterate(start)(_ + step) //.takeWhile(_ != end)
  
  def notPrime(x:BigInt) = !(isPrime(x));
  def isPrime(x:BigInt):Boolean = 
    bigIterator(BigInt(2), x-1, 1)
    .takeWhile(x%_!=0)
    .length == x-3;
  
  //def largestPrimesOf(x:Int):Int = (x-1 to 1 by -1)
//		  						   .dropWhile(y=>{x%y!=0 || notPrime(y)})
//		  						   .head;
  
  
  def largestPrimesOfI(x:BigInt):BigInt = bigIterator(x-1, BigInt(1), BigInt(-1))
  								   .dropWhile(y=>{x%y!=0 || notPrime(y)})
  								   .take(1)
  								   .sum
  def p(x: BigInt):BigInt = 
    Iterator.iterate( (BigInt(2), x) )({ case (a,b) =>  
    				if(a==b) (a, a)
    				else if(b%a==0) (a, b/a)
    				else (a+1, b)
    			})
  	.dropWhile(x => {x._1 != x._2}).map(_._1).take(1).sum;
    
  def main(args: Array[String]): Unit = {
    val x = BigInt("600851475143");
    println( largestPrimeFactor(x) );
    println("--");
    println (p(x))
  }

}