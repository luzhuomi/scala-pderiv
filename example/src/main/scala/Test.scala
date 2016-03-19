
import com.github.luzhuomi.regex.PDeriv._
import scala.io._
import java.io._



object Test
{
	def main(args:Array[String]) : Unit =
	{
    if (args.length == 2)
    {
      compile(args(0)) match
      {
        case Some(p) =>
        {
          val src = Source.fromFile(args(1)).mkString
          val res = exec(p,src.trim)

          println(res)
          /*
          for (ln <- Source.fromFile(args(1)).getLines )
          {
            val res = exec(p,ln.trim)
            res match
            {
              case None => println(ln)
              case _ => ()
            }
          }
          */
        }
        case None => println("compilation failed.")
      }
    }
    else
    {
      println("USAGE: sbt \"runMain Test <regex> <input file>\"")
    }
	}

}


/*
$ sbt "runMain Test ^[^<]*<head>.*</head>[^<]*<body>.*</body>.*</html>$ /Users/luzm/test/hwz-large.html"
*/
