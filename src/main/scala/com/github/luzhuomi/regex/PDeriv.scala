package com.github.luzhuomi.regex

import com.github.luzhuomi.scalazparsec.NonBacktracking._
import com.github.luzhuomi.regex.RE._
import com.github.luzhuomi.regex.ExtPattern._
import com.github.luzhuomi.regex.IntPattern._
import com.github.luzhuomi.regex.Parser._
import com.github.luzhuomi.regex.Translate._
import com.github.luzhuomi.regex.pderiv.LeftToRight._

object PDeriv
{
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

/*
scala> import com.github.luzhuomi.regex.PDeriv._
import com.github.luzhuomi.regex.PDeriv._

scala> val Some(p) = compile("^(ab)(c)$")
p: (com.github.luzhuomi.regex.pderiv.LeftToRightD.DPat0Table, List[Int], com.github.luzhuomi.regex.IntPattern.Binder) = (IntMap(97 -> (1,List(1),IntMap(0 -> List(<function1>))), 98 -> (2,List(),IntMap()), 99 -> (2,List(),IntMap()), 353 -> (2,List(),IntMap()), 354 -> (3,List(2),IntMap(1 -> List(<function1>))), 355 -> (2,List(),IntMap()), 609 -> (2,List(),IntMap()), 610 -> (2,List(),IntMap()), 611 -> (2,List(),IntMap()), 865 -> (2,List(),IntMap()), 866 -> (2,List(),IntMap()), 867 -> (4,List(3),IntMap(2 -> List(<function1>))), 1121 -> (2,List(),IntMap()), 1122 -> (2,List(),IntMap()), 1123 -> (2,List(),IntMap())),List(3),IntMap(0 -> List(), 1 -> List(), 2 -> List()))

scala> exec(p,"abc")
res0: Option[com.github.luzhuomi.regex.pderiv.LeftToRightD.Env] = Some(List((0,abc), (1,ab), (2,c)))

scala> val Some(p) = compile("^(ab|a)(baa|a)(ac|c)*$")
p: (com.github.luzhuomi.regex.pderiv.LeftToRightD.DPat0Table, List[Int], com.github.luzhuomi.regex.IntPattern.Binder) = (IntMap(97 -> (1,List(1, 2),IntMap(0 -> List(<function1>, <function1>))), 98 -> (2,List(),IntMap()), 99 -> (2,List(),IntMap()), 353 -> (3,List(3),IntMap(2 -> List(<function1>))), 354 -> (4,List(2, 4),IntMap(1 -> List(<function1>), 2 -> List(<function1>))), 355 -> (2,List(),IntMap()), 609 -> (2,List(),IntMap()), 610 -> (2,List(),IntMap()), 611 -> (2,List(),IntMap()), 865 -> (5,List(5),IntMap(3 -> List(<function1>))), 866 -> (2,List(),IntMap()), 867 -> (3,List(3),IntMap(3 -> List(<function1>))), 1121 -> (6,List(3, 6),IntMap(2 -> List(<function1>), 4 -> List(<function1>))), 1122 -> (7,List(4),IntMap(2 -> List(<function1>))), 1123 -> (2,List(),IntMap()), 1377 -> (2,List(),...
scala> exec(p,"abaac")
res1: Option[com.github.luzhuomi.regex.pderiv.LeftToRightD.Env] = Some(List((0,abaac), (1,ab), (2,a), (3,ac)))
*/