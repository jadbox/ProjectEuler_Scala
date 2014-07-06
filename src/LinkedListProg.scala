import scala.reflect.ClassTag;
class Node[T : ClassTag](x:T) {
  var value = x;
  var next:Option[Node[T]] = None;
}
class LL[T : ClassTag]() {
  import scala.collection.mutable.ArrayBuffer;
  import scala.collection.mutable.ListBuffer;
  var head:Option[Node[T]] = None;
  var tail:Option[Node[T]] = None;
  def push(x:T) {
    tail match {
      case(None) => {
        tail = Some(new Node(x))
        head = tail
      }
      case(Some(node)) => {
        node.next = Some(new Node(x));
        tail = node.next;
      }
    }
  }
  def pop():Option[T] =
    head match {
      case(None) => None
      case(Some(v)) => {
        if(head == tail) tail = None;
        head = v.next;
        Some(v.value);
      }
    }
  
  def toArray():Array[T] = {
    var x = ArrayBuffer[T]();
    for(y <- iterator) {
      x += y;
    }
    return x.toArray;
  }
  
  def toList():List[T] = {
    var x = ListBuffer[T]();
    for(y <- iterator) {
      x += y;
    }
    return x.toList;
  }

  def iterator = new Iterator[T] {
    var x = head;
    def hasNext = {
      x match {
        case(None) => false;
        case(Some(y)) => true;
      }
    }
    def next = {
      val r = x;
      x = x.get.next;
      r.get.value;
    }
  }
}

object LinkedListProg {
  def main(x: Array[String]) {
    var l = new LL[String]();
    l.push("test");
    l.push("test1");
    l.push("test2");
    l.push("test3");
    //l.pop();l.pop();l.pop();l.pop();l.pop();
    for(x <- l.toArray()) {
      println(x);
    }
  }
}