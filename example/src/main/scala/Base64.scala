
import com.github.luzhuomi.regex.PDeriv._
import scala.io._
import java.io._



object Base64 
{
	val opat = compile("^\\s*(?:(?:[a-zA-Z0-9+/]\\s*){4})*(?:(?:[a-zA-Z0-9+/]\\s*){2}\\s*[a-zA-Z0-9+/=]\\s*=)?\\s*$")
	def main(args:Array[String]) : Unit = 
	{
		opat match 
		{
			case Some(p) => 
			{
				println("compiled")
				// val ln = readLine()
				// val src = Source.fromFile(args(0)).getLines
				val src = Source.fromFile("/tmp/base64.txt").getLines
				for (l <- src) 
				{
					val res = exec(p,l.trim)
					 println(res)
				}

			}
			case None => println("compilation failed.")
		}
	}

}


/*
$ sbt console
*/
