
import com.github.luzhuomi.regex.PDeriv._
import scala.io._
import java.io._



object USAddressStocked 
{
	val opat = "^(.*) ([A-Za-z]{2}) ([0-9]{5})(-[0-9]{4})?$".r
	def main(args:Array[String]) : Unit = 
	{
		val ln = if (args.length > 0) { readLine() } else { "" }
		// val src = Source.fromFile(args(0)).getLines
		val src = Source.fromFile("/tmp/addr.txt").getLines
		for (l <- src) 
		{
			l match 
			{
				case opat(p1,p2,p3,p4) => println(p1,p2,p3,p4)
				case _  => println("None")
			}
		}

	}

}


/*
$ sbt console
*/
