object Problem1 {
  def multi0f35(x: Int): BigInt =
    List.range(0, x).par.aggregate(0)((t, y) => (if (y % 3 == 0 || y % 5 == 0) t + y else t), _ + _); //filter(y=>y%3==0 || y%5==0).sum;
  
  def multi0f35_2(x: Int): BigInt =
    List.range(0, x).fold(0)((t, y) => (if (y % 3 == 0 || y % 5 == 0) t + y else t)); //filter(y=>y%3==0 || y%5==0).sum;
  
  def multi0f35_3(x: Int): BigInt =
    List.range(0, x).filter(y=>y%3==0 || y%5==0).sum;
  
 def multi0f35_4(x: Int): BigInt =
    List.range(0, x).par.filter(y=>y%3==0 || y%5==0).sum;

  def main(args: Array[String]) {
    import ProblemUtils.time;
    import collection.parallel.ForkJoinTasks.defaultForkJoinPool._;
    var x = 6000;
    //setParallelism(4);
    time { println(multi0f35(x)) };
    
    time { println(multi0f35_2(x)) };
    
    time { println(multi0f35_3(x)) };
    
    time { println(multi0f35_4(x)) };
  }
}

