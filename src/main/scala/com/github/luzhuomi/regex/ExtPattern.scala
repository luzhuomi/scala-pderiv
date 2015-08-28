package com.github.luzhuomi.regex

/** The external AST
 *  
 */

object ExtPattern
{
	sealed trait EPat 
	case object EEmpty extends EPat // empty sequence
	case class EGroupNonMarking(p:EPat) extends EPat // non marking group (:?r)
	case class EGroup(p:EPat) extends EPat // the marking group (r)
	case class EOr(ps:List[EPat]) extends EPat // choice r|r
	case class EConcat(ps:List[EPat]) extends EPat // concatenation rr
	case class EOpt(p:EPat, greed:Boolean) extends EPat // option r?
	case class EPlus(p:EPat, greed:Boolean) extends EPat // one or more r+
	case class EStar(p:EPat, greed:Boolean) extends EPat // zero or more r*
	case class EBound(p:EPat, lb:Int, ub:Option[Int], greed:Boolean) extends EPat // repeat r{1:10}
	case object ECarat extends EPat // the ^
	case object EDollar extends EPat // the $
	case object EDot extends EPat // the any char .
	case class EAny(cs:List[Char]) extends EPat // the character class [ a-z ]
	case class ENoneOf(cs:List[Char]) extends EPat // the negative character class [^a-z]
	case class EEscape(c:Char) extends EPat // the backslash character
	case class EChar(c:Char) extends EPat // the non-escaped character


	def hasGroup(p:EPat) : Boolean = p match 
	{
		case EEmpty 			  => false
		case EGroup(_) 			  => true
		case EGroupNonMarking(ep) => hasGroup(ep)
		case EOr(eps) 			  => eps exists hasGroup
		case EConcat(eps) 		  => eps exists hasGroup
		case EOpt(ep,_) 		  => hasGroup(ep) 
		case EPlus(ep,_) 		  => hasGroup(ep)
		case EStar(ep,_) 		  => hasGroup(ep)
		case EBound(ep,_,_,_)     => hasGroup(ep)
		case ECarat 			  => false
		case EDollar			  => false
		case EDot 				  => false
		case EAny(_)			  => false
		case ENoneOf(_)			  => false
		case EEscape(_)			  => false
		case EChar(_)			  => false
	}

}