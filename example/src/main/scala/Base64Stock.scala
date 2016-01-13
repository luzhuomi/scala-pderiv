
import scala.io._
import java.io._
// using the builtin regex in Scala
import scala.util.matching.Regex

object Base64Stock 
{
	val opat = "^\\s*(?:(?:[a-zA-Z0-9+/]\\s*){4})*(?:(?:[a-zA-Z0-9+/]\\s*){2}\\s*[a-zA-Z0-9+/=]\\s*=)?\\s*$".r
	def main(args:Array[String]) : Unit = 
	{
		// val src = Source.fromFile(args(0)).getLines
		val src = Source.fromFile("/tmp/base64.txt").getLines
		for (l <- src) 
		{
			l.trim match 
			{
				case opat(_*) => println ("matched")
				case _ => println ("not matched")
			}
		}
	}

}


/*
$ sbt console
*/
