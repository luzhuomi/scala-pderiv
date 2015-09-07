package com.github.luzhuomi.scalapderivexample

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


/*
$ sbt console
*/
