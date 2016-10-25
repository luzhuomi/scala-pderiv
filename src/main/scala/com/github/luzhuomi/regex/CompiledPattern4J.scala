package com.github.luzhuomi.regex

// Interface With Java

import com.github.luzhuomi.scalazparsec.NonBacktracking._
import com.github.luzhuomi.regex.pderiv.RE._
import com.github.luzhuomi.regex.pderiv.ExtPattern._
import com.github.luzhuomi.regex.pderiv.IntPattern._
import com.github.luzhuomi.regex.pderiv.Parser._
import com.github.luzhuomi.regex.pderiv.Translate._
import com.github.luzhuomi.regex.PDeriv
import scala.collection.JavaConversions._

class CompiledPattern4J(regex:String)
{
	val compiled = PDeriv.compile (regex)
	def isSucc:Boolean = compiled match 
	{
		case Some(_) => true
		case None    => false
	}
	def exec_(w:String):List[(Int,String)] = compiled match 
	{
		case Some(cp) => PDeriv.exec(cp,w) match 
		{
			case None      => Nil
			case Some(env) => env
		}
		case None     => Nil
	}
	def exec(w:String):java.util.List[String] = exec_(w).map(_._2)
}

