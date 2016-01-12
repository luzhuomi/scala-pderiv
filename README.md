# scala-pderiv
Regular expression parser implemented in parsec and matcher implemented using partial derivatives.

# theory
Refer to our ppdp 12 paper.
https://sites.google.com/site/luzhuomi/file/ppdp39-sulzmann.pdf?attredirects=0

# example
For Scala example, refer to example/src/scala/
```
import com.github.luzhuomi.regex.PDeriv._
import scala.io._
import java.io._

object USAddress 
{
	val opat = compile("^(.*) ([A-Za-z]{2}) ([0-9]{5})(-[0-9]{4})?$")
	def main(args:Array[String]) : Unit = 
	{
		opat match 
		{
			case Some(p) => 
			{
				// println("compiled")
				// val src = Source.fromFile(args(0)).getLines
				val src = Source.fromFile("/tmp/addr.txt").getLines
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
```

For Java example, refer to this sub project
```
https://github.com/luzhuomi/scala-pderiv-java-ex
```
