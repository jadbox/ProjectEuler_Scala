object ConvTest {
  def med[T <: AnyVal{ def toInt:Int;} ](x:Array[T]) = {
    ((x(x.length/2-1).toInt + x(x.length/2).toInt)/2).asInstanceOf[T]
  }
  
  def main(args: Array[String]): Unit = {
    var l = Array[Char]('a', 'b', 'd', 'e')
    l = l.sorted
    println ( med(l) );
    
    var x = Array[Int](1, 2, 5, 6)
    x = x.sorted
    println ( med(x) );
  }

}