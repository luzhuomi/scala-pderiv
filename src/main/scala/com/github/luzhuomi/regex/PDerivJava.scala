package com.github.luzhuomi.regex

// Interface With Java

import com.github.luzhuomi.scalazparsec.NonBacktracking._
import com.github.luzhuomi.regex.RE._
import com.github.luzhuomi.regex.ExtPattern._
import com.github.luzhuomi.regex.IntPattern._
import com.github.luzhuomi.regex.Parser._
import com.github.luzhuomi.regex.Translate._
import com.github.luzhuomi.regex.pderiv.LeftToRightD._

object PDerivJava
{

	class CompiledPattern
	{
		val compiled
	}

	def compile(regex:String):Option[CompiledPat] = 
	{
		parseEPat(regex) match 
		{
			case Consumed(Some((ep,Nil))) => 
			{
				val p = translate(ep)
				Some(compilePat(p))
			} 
			case _ => None // compilation fail
		}
	}

	def exec(cp:CompiledPat,w:Word):Option[Env] = 
	{
		greedyPatMatchCompiled(cp,w)
	}
}