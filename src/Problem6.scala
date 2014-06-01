object Problem6 {

 
  def main(args: Array[String]): Unit = {
	val x = math.pow((1 to 10).sum, 2);	  
    val s = (1 to 10).map(math.pow(_,2)).sum;
    
    val y = (1 to 100).map(x=>(x:BigInt,x:BigInt)).map( t=>(t._1*t._1,t._2) ).
    		reduceLeft((t,y)=>(t._1+y._1,t._2+y._2) );
		  
	println( y._2*y._2-y._1 );
  }

}