# scala-pderiv
Regular expression parser implemented in parsec and matcher implemented using partial derivatives.

# theory
Refer to our ppdp 12 paper.
https://sites.google.com/site/luzhuomi/file/ppdp39-sulzmann.pdf

# example
For Scala example, refer to example/src/scala/
```scala
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
# limitation
Most of the escape characters except for (\s) are not yet supported. I will fix them when time permits. 

# performance
Optimization in progress. 

# credits

Thanks to the Yourkit team who is willing to provide a opensource license for their profiler.
YourKit supports open source projects with its full-featured Java Profiler.

<img src="https://www.yourkit.com/images/yklogo.png"/>
YourKit, LLC is the creator of <a href="https://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a>
and <a href="https://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>,
innovative and intelligent tools for profiling Java and .NET applications. 
