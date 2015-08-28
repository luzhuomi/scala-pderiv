package com.github.luzhuomi.regex

/** The internal AST
 *  
 */

import com.github.luzhuomi.regex.Common._
import com.github.luzhuomi.regex.RE._

object IntPattern
{
	sealed trait Pat 
	case class PVar(x:Int,range:List[Range],p:Pat) extends Pat // variable pattern
	case class PE(r:RE) extends Pat
	case class PPair(p1:Pat,p2:Pat) extends Pat
	case class PChoice(p1:Pat, p2:Pat, gf:GFlag) extends Pat
	case class PStar(p:Pat, gf:GFlag) extends Pat 
	case class PPlus(p:Pat, ps:Pat) extends Pat // used only internnaly to indicate that it is unrolled from a PStar
	case class PEmpty(p:Pat) extends Pat // empty pattern, used internally to marked that mkEmpty function has been applied to this pattern


	def strip(p:Pat) : RE = p match 
	{
		case PVar(_,w,p)       => strip(p)
		case PE(r)             => r
		case PStar(p,g)        => Star(strip(p),g)
		case PPair(p1,p2)      => Seq(strip(p1),strip(p2))
		case PPlus(p1,p2)      => Seq(strip(p1),strip(p2))
		case PChoice(p1,p2,gf) => Choice(strip(p1), strip(p2),gf)
		case PEmpty(p)         => strip(p)
	}
}
