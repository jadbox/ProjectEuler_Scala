object Problem7 {

  lazy val primes2:Stream[Int] =
    2 #:: Stream.from(3).filter(i =>
    		primes2.takeWhile(j => j * j <= i).forall(i % _ != 0))
  
  def primes() = 
      Stream.from(3).iterator dropWhile(y=> 
      	(1 to y).filter(y%_==0).length!=2)
      

  def main(args: Array[String]): Unit = {
    //val p = primes();
    //(1 to 13).foreach(x=>{p.next()});
    //println( p.next());
    println( primes2(10000) );
    
  }

}