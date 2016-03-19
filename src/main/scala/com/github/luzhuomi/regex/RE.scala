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

	def sigmaRE(r:RE) : List[Char] =
	{
		sigmaREsub(r).toList
	}

	def sigmaREsub(r:RE) : Set[Char] = r match
	{
		case L(l) => Set(l)
		case Any  => (0 to 127).map(_.toChar).toSet // (32 to 127).map(_.toChar).toSet
		case Not(cs) =>
		{
			val s = cs.toSet
			// (32 to 127).map(_.toChar).filter( s.contains(_)).toSet
			(0 to 127).map(_.toChar).filter( s.contains(_)).toSet
		}
		case Seq(r1,r2) => sigmaREsub(r1) union sigmaREsub(r2)
		case Choice(r1,r2,g) => sigmaREsub(r1) union sigmaREsub(r2)
		case Star(r,g) => sigmaREsub(r)
		case Phi => Set()
		case Empty => Set()
	}

	def resToRE(l:List[RE]):RE = l match {
		case Nil => Phi
		case (r::rs) => rs.foldLeft(r)( (r1:RE,r2:RE) => Choice(r1,r2,Greedy))
	}


	implicit object REIsGreedy extends IsGreedy [RE]
	{
		def isGreedy(r:RE):Boolean = r match
		{
			case Phi 			 => false
			case Empty 			 => false
			case Choice(r1,r2,Greedy) => true
			case Choice(r1,r2,NotGreedy) => false
			case Seq(r1,r2)      => isGreedy(r1) || isGreedy(r2)
			case Star(_,Greedy) 	 => true
			case Star(_,NotGreedy) 	 => false
			case L(_) 			 => false
			case Any 			 => false
			case Not(_) 		 => false
		}
	}


	implicit object REPosEpsilon extends PosEpsilon[RE]
	{
		def posEpsilon(r:RE):Boolean = r match
		{
			case Phi 			 => false
			case Empty 			 => true
			case Choice(r1,r2,g) => posEpsilon(r1) || posEpsilon(r2)
			case Seq(r1,r2)      => posEpsilon(r1) && posEpsilon(r2)
			case Star(Phi,_) 	 => true
			case Star(r,g)  	 => true
			case L(_) 			 => false
			case Any 			 => false
			case Not(_) 		 => false
		}
	}


	implicit object REIsEpsilon extends IsEpsilon[RE]
	{
		def isEpsilon(r:RE):Boolean = r match
		{
			case Phi 			 => false
			case Empty 			 => true
			case Choice(r1,r2,g) => isEpsilon(r1) && isEpsilon(r2)
			case Seq(r1,r2)      => isEpsilon(r1) && isEpsilon(r2)
			case Star(Phi,_) 	 => true
			case Star(r,g)  	 => isEpsilon(r)
			case L(_) 			 => false
			case Any 			 => false
			case Not(_) 		 => false
		}
	}

	implicit object REisPhi extends IsPhi[RE]
	{
		def isPhi(r:RE):Boolean = r match
		{
			case Phi 			 => true
			case Empty 			 => false
			case Choice(r1,r2,g) => isPhi(r1) && isPhi(r2)
			case Seq(r1,r2)      => isPhi(r1) || isPhi(r2)
			case Star(Phi,_) 	 => false
			case Star(r,g)  	 => false
			case L(_) 			 => false
			case Any 			 => false
			case Not(_) 		 => false
		}
	}

	implicit object RESimplifiable extends Simplifiable[RE]
	{
		def simplify(r:RE):RE = r match
		{
			case L(l) => L(l)
			case Any => Any
			case Not(cs) => Not(cs)
			case Seq(r1,r2) =>
			{
				val r1p = simplify(r1)
				val r2p = simplify(r2)
				if (implicitly[IsEpsilon[RE]].isEpsilon(r1p)) { r2p }
				else
				{
					if (implicitly[IsEpsilon[RE]].isEpsilon(r2p)) { r1p }
					else
					{
						Seq(r1p,r2p)
					}
				}
			}
			case Choice(r1,r2,g) =>
			{
				val r1p = simplify(r1)
				val r2p = simplify(r2)
				if (implicitly[IsPhi[RE]].isPhi(r1p)) { r2p }
				else
				{
					if (implicitly[IsPhi[RE]].isPhi(r2p)) { r1p }
					else
					{
						Choice(r1p,r2p,g)
					}
				}
			}
			case Star(r,g) =>
			{
				Star(simplify(r),g)
			}
			case Phi => Phi
			case Empty => Empty
		}
	}

	def pd(r:RE,l:Char):List[RE] =
	{
		val pds = pdSub(r,l)
		pds.distinct
	}

	def pdSub(r:RE,l:Char):List[RE] = r match
	{
		case Phi => Nil
		case Empty => Nil
		case L(lp) if lp == l => List(Empty)
		case L(lp) 			  => Nil

		case Any => List(Empty)
		case Not(cs) if cs.contains(l) => List(Empty)
		case Not(cs) 			  	   => Nil
		case Choice(r1,r2,g) => pdSub(r1,l) ++ pdSub(r2,l)
		case Seq(r1,r2) if (implicitly[PosEpsilon[RE]].posEpsilon(r1)) =>
		{
			val s0 = pdSub(r1,l)
			val s1 = s0 map ( r => Seq(r,r2))
			val s2 = pdSub(r2,l)
			s1 ++ s2
		}
		case Seq(r1,r2) =>
		{
			val s0 = pdSub(r1,l)
			val s1 = s0 map ( r => Seq(r,r2))
			s1
		}
		case Star(r,g) =>
		{
			val s0 = pdSub(r,l)
			val s1 = s0 map ( r1 => Seq(r1,Star(r,g)))
			s1
		}
	}


}
