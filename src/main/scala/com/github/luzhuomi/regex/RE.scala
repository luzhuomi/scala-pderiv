package com.github.luzhuomi.regex

import com.github.luzhuomi.regex.Common._

object RE
{
	sealed trait RE 
	case object Phi extends RE
	case object Empty extends RE
	case class L(c:Char) extends RE
	case class Choice(r1:RE,r2:RE,gf:GFlag) extends RE
	case class Seq(r1:RE,r2:RE) extends RE
	case class Star(r:RE,gf:GFlag) extends RE
	case object Any extends RE
	case class Not(cs:List[Char]) extends RE

}