object Problem4 {
  import ProblemUtils.time;
  
  def isPalindrom( x:Int ):Boolean = {
    val s = x.toString();
	s==s.reverse;
  }
  
  def twoMultiples() = 
    for(
        a <- 999 to 100 by -1;
        b <- a to 100 by -1
    ) yield (a,b,a*b)
  
  def twoMultiples2() = 
    for(
        z <- 0 to 998 by 1;
        a <- 999 to 100 by -1;
        b <- a-z to a-z-1 by -1
    ) yield (a,b,a*b)
    
    //102 // 2 // 1
  def numToList(x:Int) = 
      (math.log10(x).floor to 0L by -1L).
      		map(y =>( (if(y==0) x else x/math.pow(10,y) )%10).toLong);
  
  def isPalindrom2( x:Int ):Boolean = {
    val s = numToList(x);
	s==s.reverse;
  }
  
  def main(args: Array[String]): Unit = {
    
    time{ 
      val x = (100 to 999).view
        .flatMap(i => (i to 999).map(i *))
        .filter(n => n.toString == n.toString.reverse)
        .max 
      println(x);
    }
    
    time{
      val pals = twoMultiples() filter ((x)=>{isPalindrom(x._3)})
      println(pals.maxBy(_._3))
    }
    
    time{ 
      val x = (999 to 100 by -1).view
        .flatMap(i => (i to 100 by -1).map(i *))
        .filter(isPalindrom)
        .max 
      println(x);
    }
  }

}