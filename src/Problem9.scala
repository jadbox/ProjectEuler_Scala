object Problem9 {
 // lazy val p2 = (0,1,1) #: (0)
  def p():IndexedSeq[(Int,Int, Double)] = 
    for(
        a <- (1 to 499);
        b <- (a+1 to 499)
    ) yield (a,b, math.sqrt((math.pow(a,2)+math.pow(b,2))))
  
  
  def main(args: Array[String]): Unit = {
    val s = p().find(x=> x._1+x._2+x._3==1000d ).get;
    println( s );
    var re = BigInt(s._1) * BigInt(s._2) * BigInt(s._3.toInt);
    println( re );
  }

}