object Problem12 {
  
  val naturalsIter =
    Iterator.iterate((1,1))(x=>(x._1+1, x._1+x._2+1)).map(_._2)
  
  lazy val naturalsStr:Stream[Int] = 
    0 #:: naturalsStr.zipWithIndex.map(p=>p._1+p._2+1)
  
  def factors(x:Int) = 
    (1 to x).takeWhile(y=> y*y <= x)
    .filter(x%_==0).length*2

  def main(args: Array[String]): Unit = {
    import ProblemUtils.time;
    
    println( factors(13) )
    println( factors(6) )
    println( factors(28) )
    time {
     val r = naturalsIter.find(x => factors(x) > 500).get;
     println(r);
    }
   
   // n.take(4).foreach(println);
    
  }

}