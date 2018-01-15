import org.apache.log4j.Logger

class Operation {

  def rotateN(myList: List[Char], n: Int): List[Char] = {
    n match {
      case 0 => myList
      case _ => {
        myList match {
          case head :: tail => rotateN(tail :+ head, n - 1)
          case _ => rotateN(myList, n - 1)
        }
      }
    }
  }

  def eliminateDuplicates(myList: List[Char], myList1: List[Char]): List[Char] = {
    myList match {
      case head :: head1 :: tail if (head == head1) => eliminateDuplicates(head :: tail, myList1)
      case head :: head1 :: tail if (head != head1) => eliminateDuplicates(head1 :: tail, myList1 :+ head)
      case head :: Nil => myList1 :+ head
      case _ => myList1
    }
  }

  def happySad(n: Int, list: List[Int]): Boolean = {
    def sum(n: Int, s: Int): Int = {
      n match {
        case 0 => s
        case _ => sum(n / 10, s + (n % 10) * (n % 10))
      }
    }

    def check(myList: List[Int], number: Int): Boolean = {
      myList match {
        case head :: tail if (head != number) => check(tail, number)
        case head :: tail if (head == number) => true
        case head :: Nil if (head != number) => false
        case head :: Nil if (head == number) => true
        case _ => false
      }
    }

    val num = sum(n, 0)
    num match {
      case 1 => true
      case _ => {
        check(list, num) match {
          case true => false
          case false => happySad(num, num :: list)
        }
      }
    }
  }
}

object OperationTest extends App {
  val obj = new Operation
  val log = Logger.getLogger(this.getClass)
  val list = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')
  val list1 = List('b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e', 'a', 'a', 'a', 'a')
  log.info(obj.eliminateDuplicates(list1, List()) + "\n")
  val n = 3
  val num = 20
  obj.happySad(num, List()) match {
    case false => log.info("sad Number" + "\n")
    case true => log.info("Happy Number" + "\n")
  }
  log.info(obj.rotateN(list, n))

}
