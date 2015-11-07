package com.github.luzhuomi.regex

/** The external AST
 *  
 */

object ExtPattern
{
	sealed trait EPat 
	case class EEmpty(loc:Loc) extends EPat // empty sequence
	case class EGroupNonMarking(p:EPat,loc:Loc) extends EPat // non marking group (:?r)
	case class EGroup(p:EPat,loc:Loc) extends EPat // the marking group (r)
	case class EOr(ps:List[EPat],loc:Loc) extends EPat // choice r|r
	case class EConcat(ps:List[EPat],loc:Loc) extends EPat // concatenation rr
	case class EOpt(p:EPat, greed:Boolean, loc:Loc) extends EPat // option r?
	case class EPlus(p:EPat, greed:Boolean, loc:Loc) extends EPat // one or more r+
	case class EStar(p:EPat, greed:Boolean, loc:Loc) extends EPat // zero or more r*
	case class EBound(p:EPat, lb:Int, ub:Option[Int], greed:Boolean, loc:Loc) extends EPat // repeat r{1:10}
	case class ECarat(loc:Loc) extends EPat // the ^
	case class EDollar(loc:Loc) extends EPat // the $
	case class EDot(loc:Loc) extends EPat // the any char .
	case class EAny(cs:List[Char],loc:Loc) extends EPat // the character class [ a-z ]
	case class ENoneOf(cs:List[Char],loc:Loc) extends EPat // the negative character class [^a-z]
	case class EEscape(c:Char,loc:Loc) extends EPat // the backslash character
	case class EChar(c:Char,loc:Loc) extends EPat // the non-escaped character

	// the src loc w.r.t ot the right most character
	type Loc = (Int,Int) // start,end

	def hasGroup(p:EPat) : Boolean = p match 
	{
		case EEmpty(_) 			      => false
		case EGroup(_,_) 			  => true
		case EGroupNonMarking(ep,_)   => hasGroup(ep)
		case EOr(eps,_) 			  => eps exists hasGroup
		case EConcat(eps,_) 		  => eps exists hasGroup
		case EOpt(ep,_,_) 		  	  => hasGroup(ep) 
		case EPlus(ep,_,_) 		  	  => hasGroup(ep)
		case EStar(ep,_,_) 		  	  => hasGroup(ep)
		case EBound(ep,_,_,_,_)       => hasGroup(ep)
		case ECarat(_) 			  	  => false
		case EDollar(_)			  	  => false
		case EDot(_) 			  	  => false
		case EAny(_,_)			  	  => false
		case ENoneOf(_,_)			  => false
		case EEscape(_,_)			  => false
		case EChar(_,_)			  	  => false
	}

}