package com.github.luzhuomi.regex

/** The internal AST
 *  
 */



import scala.collection.immutable.IntMap
import com.github.luzhuomi.regex.Common._
import com.github.luzhuomi.regex.RE._

object IntPattern
{
	sealed trait Pat 
	case class PVar(x:Int,range:List[(Int,Int)],p:Pat) extends Pat // variable pattern
	case class PE(r:RE) extends Pat
	case class PPair(p1:Pat,p2:Pat) extends Pat
	case class PChoice(p1:Pat, p2:Pat, gf:GFlag) extends Pat
	case class PStar(p:Pat, gf:GFlag) extends Pat 
	case class PPlus(p:Pat, ps:Pat) extends Pat // used only internnaly to indicate that it is unrolled from a PStar
	case class PEmpty(p:Pat) extends Pat // empty pattern, used internally to marked that mkEmpty function has been applied to this pattern

	implicit object PatIsGreedy extends IsGreedy[Pat]
	{
		def isGreedy(p:Pat):Boolean = p match 
		{
			case PVar(i,rs,p) => isGreedy(p)
			case PPair(p1,p2) => isGreedy(p1) || isGreedy(p2)
			case PChoice(p1,p2,Greedy) => true
			case PChoice(p1,p2,NotGreedy) => false
			case PStar(p,Greedy) => true
			case PStar(p,NotGreedy) => false
			case PPlus(p1,p2) => isGreedy(p1) || isGreedy(p2)
			case PE(r) => implicitly[IsGreedy[RE]].isGreedy(r)
		}
	}

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

	type Binder = IntMap[List[(Int,Int)]]

	def toBinder(p:Pat):Binder = 
	{ 
		val em:Binder = IntMap.empty
		toBinderList(p).foldLeft(em)( (im,kv) => im + kv )
	}

	def toBinderList(p:Pat):List[(Int,List[(Int,Int)])] = p match 
	{
		case PVar(i,rs,p)     => (i,rs)::toBinderList(p)
		case PE(r)            => Nil
		case PPair(p1,p2)     => toBinderList(p1) ++ toBinderList(p2)
		case PPlus(p1,p2)     => toBinderList(p1) ++ toBinderList(p2)
		case PStar(p,g)       => toBinderList(p)
		case PChoice(p1,p2,g) => toBinderList(p1) ++ toBinderList(p2)
		case PEmpty(p)        => toBinderList(p)
	} 

	def listify(b:Binder):List[(Int,List[Range])] = b.toList.sortWith( (x,y) => (x._1 < y._1) )


	def pdPat0Sim(p:Pat, l:Letter)(implicit s:Simplifiable[Pat]):List[(Pat,Int => Binder => Binder)] =
	{
		val pfs = pdPat0(p,l)
		val pfsp = pfs map ( pf => (s.simplify(pf._1),pf._2))
		nub2(pfsp)
	}

	def nub2[A,B](l:List[(A,B)]):List[(A,B)] = 
	{
		def go[A,B](l:List[(A,B)],s:Set[A],acc:List[(A,B)]) : List[(A,B)] = l match 
		{
			case Nil => acc.reverse
			case (x::xs) if s.contains(x._1) => go(xs,s,acc)
			case (x::xs)                     => go(xs,s+x._1,x::acc)
		}

		go(l,Set.empty,Nil)
	}

	def pdPat0(p:Pat, l:Letter):List[(Pat,Int => Binder => Binder)] = p match 
	{
		case PVar(x,w,p) if hasBinder(p) =>
		{
			def g:Int=>Binder=>Binder = updateBinderByIndex(x)
			val pfs = pdPat0(p,l)
			for 
			{
				(pd,f) <- pfs 
			} yield (PVar(x,Nil,pd), ((i:Int) => (b:Binder) => g(i)(f(i)(b))))
		}
		case PVar(x,w,p)  =>
		{
			def g:Int=>Binder=>Binder = updateBinderByIndex(x)
			val pds = pd(strip(p),l._1)
			if (pds.isEmpty) { Nil }
			else 
			{
				for 
				{
					pd <- pds
				} yield (PVar(x,Nil,PE(pd)),g)
			}
		}
		case PE(r) => 
		{
			val pds = pd(r,l._1)
			if (pds.isEmpty) { Nil }
			else 
			{
				List((PE(resToRE(pds)), ( (i:Int) => (x=>x) )))
			}
		}
		case PStar(p,g) => 	
		{
			val pfs = pdPat0(p,l)
			for 
			{
				(pp, f) <- pfs 
			} yield (PPair(pp,PStar(p,g)),f)
		}
		case PPair(p1,p2) =>
		{
			if (implicitly[PosEpsilon[RE]].posEpsilon(strip(p1)))
			{
				val pfs1 = for 
				{
					(p1p,f) <- pdPat0(p1,l)
				} yield (PPair(p1p,p2),f) 

				if (implicitly[IsGreedy[Pat]].isGreedy(p1))
				{
					nub2(pfs1++pdPat0(p2,l))
				} else 
				{
					nub2(pdPat0(p2,l)++pfs1)
				}
			} else 
			{
				for 
				{
					(p1p,f) <- pdPat0(p1,l)
				} yield (PPair(p1p,p2),f) 
			}
		}
		case PChoice(p1,p2,g) => 
		{
			nub2(pdPat0(p1,l)++pdPat0(p2,l))
		}
	}

	def hasBinder(p:Pat):Boolean = p match
	{
		case PVar(_,_,_) => true
		case PPair(p1,p2) => hasBinder(p1) || hasBinder(p2)
		case PPlus(p1,p2) => hasBinder(p1)
		case PStar(p,g)   => hasBinder(p)
		case PE(_)        => false
		case PChoice(p1,p2,_) => hasBinder(p1) || hasBinder(p2)
		case PEmpty(p)        => hasBinder(p)
	}




	implicit object PatIsEpsilon extends IsEpsilon[Pat]
	{
		def isEpsilon(p:Pat):Boolean = p match 
		{
			case PVar(i,rs,p) => isEpsilon(p)
			case PPair(p1,p2) => isEpsilon(p1) && isEpsilon(p2)
			case PChoice(p1,p2,g) => isEpsilon(p1) || isEpsilon(p2)
			case PStar(p,_) => isEpsilon(p)
			case PPlus(p1,p2) => isEpsilon(p1) && isEpsilon(p2)
			case PE(r) => implicitly[IsEpsilon[RE]].isEpsilon(r)
			case PEmpty(r) => true
		}
	}

	implicit object PatIsPhi extends IsPhi[Pat]
	{
		def isPhi(p:Pat):Boolean = p match
		{
			case PVar(i,rs,p) => isPhi(p)
			case PPair(p1,p2) => isPhi(p1) || isPhi(p2)
			case PChoice(p1,p2,g) => isPhi(p1) && isPhi(p2)
			case PStar(p,_) => isPhi(p)
			case PPlus(p1,p2) => isPhi(p1) || isPhi(p2)
			case PE(r) => implicitly[IsPhi[RE]].isPhi(r)
			case PEmpty(p) => false
		}
	}

	implicit object PatSimplifiable extends Simplifiable[Pat] 
	{
		def simplify(p:Pat):Pat = p match 
		{
			case PVar(i,rs,p) => PVar(i,rs,simplify(p))
			case PPair(p1,p2) => 
			{
				val p1p = simplify(p1)
				val p2p = simplify(p2)
				if (implicitly[IsEpsilon[Pat]].isEpsilon(p1p)) { p2p }
				else 
				{
					if (implicitly[IsEpsilon[Pat]].isEpsilon(p2p)) { p1p }
					else { PPair(p1p,p2p) }
				}
			}
			case PChoice(p1,p2,g) =>
			{
				val p1p = simplify(p1)
				val p2p = simplify(p2)
				if (implicitly[IsPhi[Pat]].isPhi(p1p)) { p2p }
				else 
				{
					if (implicitly[IsPhi[Pat]].isPhi(p2p)) { p1p }
					else { PChoice(p1p,p2p,g) }
				}
			}
			case PStar(p,g)   => PStar(simplify(p),g)
			case PPlus(p1,p2) => PPlus(simplify(p1), simplify(p2))
			case PE(r) 		  => PE(implicitly[Simplifiable[RE]].simplify(r))
			case PEmpty(p)    => PEmpty(simplify(p))
		}
	}
	// update a binder given an index to a pattern variable
	// assumption: the var index in the pattern is linear
	def updateBinderByIndex(i:Int)(pos:Int)(binder:Binder):Binder = 
	{
		binder.get(i) match
		{
			case None => binder.updated(i,List((pos,pos)))
			case Some(Nil) => binder.updated(i,List((pos,pos)))
			case Some((b,e)::rs) => 
			{
				val ep = e + 1
				val vp = if (pos == ep) { (b,ep)::rs } else { (pos,pos)::(b,e)::rs } /* if (pos > ep)  implicitly,  it is impossible that pos < ep */ 
				binder.updated(i,vp)
			}
		}
	}
}
