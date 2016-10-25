package com.github.luzhuomi.regex.pderiv

/**
* Translating from External AST to the Internal AST.
*
*/ 

import scalaz._
import scalaz.{ State => S }
import Scalaz._
import com.github.luzhuomi.regex.pderiv.ExtPattern._
import com.github.luzhuomi.regex.pderiv.IntPattern._
import com.github.luzhuomi.regex.pderiv.RE._
import com.github.luzhuomi.regex.pderiv.Common._

object Translate 
{

	case class TState ( ngi:NGI, gi:GI, anchorStart:Boolean, anchorEnd:Boolean ) 

	// 0, -1, -2 are reserved for pre-, main- and post- subexpressions, for (un)anchored expression
	val initTState = TState(-3,1,false,false)

	type NGI = Int
	type GI = Int

	// get NGI

	def getNGI:State[TState,NGI] = for 
	{
		st <- get
	} yield st.ngi


	// get then increase
	def getIncNGI:State[TState,NGI] = for 
	{
		st <- get[TState] // local inference problem
		val n = st.ngi
		val st_ = st.copy(ngi= n+1)
		_ <- put(st_)
	} yield n


	// get GI

	def getGI:State[TState,GI] = for 
	{
		st <- get
	} yield st.gi


	// get then increase
	def getIncGI:State[TState,GI] = for 
	{
		st <- get[TState]
		val g = st.gi
		val st_ = st.copy(gi=g+1)
		_ <- put(st_)
	} yield g

	def getAnchorStart:State[TState,Boolean] = for 
	{
		st <- get
	} yield st.anchorStart

	def setAnchorStart:State[TState,Unit] = for 
	{
		st <- get
		_ <- put(st.copy(anchorStart = true))
	} yield ()

	def getAnchorEnd:State[TState,Boolean] = for 
	{
		st <- get
	} yield st.anchorEnd

	def setAnchorEnd:State[TState,Unit] = for 
	{
		st <- get
		_ <- put(st.copy(anchorEnd = true))
	} yield ()


	// translating external pattern to internal patatern

	def translate(epat:EPat):Pat =  trans(epat).run(initTState) match 
	{
		case ((state,pat)) => 
		{
			val hasAnchorS = state.anchorStart
			val hasAnchorE = state.anchorEnd
			(hasAnchorS,hasAnchorE) match 
			{
				case (true,true) => PVar(mainBinder,Nil,pat)
				case (true,false) => PPair(PVar(mainBinder,Nil,pat), PVar(subBinder,Nil,PE(Star(Any,NotGreedy))))
				case (false,true) => PPair(PVar(preBinder,Nil,PE(Star(Any,NotGreedy))), PVar(mainBinder,Nil,pat)) 
				case (false,false) => PPair(PVar(preBinder,Nil,PE(Star(Any,NotGreedy))), PPair(PVar(mainBinder,Nil,pat),PVar(subBinder,Nil,PE(Star(Any,NotGreedy))))) 
			}
		}
	}

	def trans(epat:EPat):State[TState,Pat] = 
	{
		if (hasGroup(epat))
		{
			p_trans(epat)
		}
		else 
		{
			for 
			{
				r <- r_trans(epat)
			} yield (PE(r))
		}
	}
	// making a RE out of a sequence of REs by concatenation
	def mkSeq(l:List[RE]):RE = l match 
	{
		case Nil => Empty
		case (r::rs) => rs.foldLeft(r)( (x:RE,y:RE) => Seq(x,y) )
	}	

	// making a RE out of a list of character via choice 
	def char_list_to_re(l:List[Char]):RE = l match
	{
		case Nil => Empty
		case (c::cs) => cs.foldLeft(L(c):RE)( (r:RE,c:Char) => RE.Choice(r,L(c),Greedy) )
	}

	// a tricky wrapper to have point with a slightly more specific instance of Monad State
	def point[A](a: => A)(implicit m:Monad[({type λ[B] = State[TState,B]})#λ]):State[TState,A] = m.point(a)

	def p_trans(epat:EPat):State[TState,Pat] = epat match 
	{
		// () ~>_p ()
		case EEmpty(_) => point(PE(Empty))
		/**
		* e ~> p
		* ------------------
		* ( e ) ~>_p x :: p
		*/
		case EGroup(e,_) => for 
		{
			i <- getIncGI
			p <- trans(e)
		} yield PVar(i,Nil,p)
		case EGroupNonMarking(e,_) => trans(e)
		case EOr(es,_) => 
		{ 
			def mkChoice(ps:List[Pat]) : Pat = ps match 
			{
				case (p::ps_) => ps_.foldLeft(p)( (p1:Pat,p2:Pat) => PChoice(p1,p2,Greedy))
				case Nil => PE (Phi)
			}
			for 
			{
				ps <- es.traverseS(trans(_))
			} yield mkChoice(ps)
		}
		case EConcat(es,_) => for
		{
			ps <- es.traverseS(trans(_))
			val q = ps.reverse match 
			{
				case (pp::pps) => pps.foldLeft(pp) ( (p1:Pat, p2:Pat) => PPair (p2,p1) )
				case Nil => PE (Phi)
			}
		} yield q
		case EOpt(e,b,_) => for 
		{
			p <- trans(e)
			val g = if (b) { Greedy } else { NotGreedy }
		} yield PChoice(p,PE(Empty),g)
		case EPlus(e,b,_) => for 
		{
			p <- trans(e)
			val g = if (b) { Greedy } else { NotGreedy }
		} yield PPair(p,PStar(p,g))
		case EStar(e,b,_) => for 
		{
			p <- trans(e)
			val g = if (b) { Greedy } else { NotGreedy }
		} yield PStar(p,g)
		case EBound(e,low,Some(high),b,_) => for 
		{
			r <- r_trans(e)
			i <- getIncNGI
			val g = if (b) { Greedy } else { NotGreedy }
			val r1s = Nil.padTo(low,r)
			val r1  = mkSeq(r1s)
			val r2s = Nil.padTo(high-low,RE.Choice(r,Empty,g))
			val r2  = mkSeq(r2s)
			val r3 = (r1,r2) match 
			{
				case (Empty,Empty) => Empty
				case (Empty,_)     => r2
				case (_    ,Empty) => r1
				case (_    ,_)     => Seq(r1,r2)
			}
			val p = PVar(i,Nil,PE(r3))
		} yield p
		case EBound(e,low,None,b,_) => for 
		{
			r <- r_trans(e)
			i <- getIncNGI
			val g = if (b) { Greedy } else { NotGreedy }
			val r1s = Nil.padTo(low,r)
			val r1  = mkSeq(r1s)
			val r2  = Seq(r1,Star(r,g))
			val p   = PVar(i, Nil, PE(r2))
		}  yield p
		case ECarat(_) => for 
		{
			f <- getAnchorStart
			x <- if (f) 
			{
				for 
				{
					i <- getIncNGI
				} yield PVar(i,Nil,PE(L('^')))
			} else 
			{
				for 
				{
					_ <- setAnchorStart
					i <- getIncNGI
				} yield PVar(i,Nil,PE(Empty))
			}
		} yield x
		case EDollar(_) => for 
		{
			f <- getAnchorEnd
			_ <- if (f) 
			{
				point(())
			} else 
			{
				setAnchorEnd
			}
			i <- getIncNGI
			val p = PVar(i,Nil,PE(Empty))
		} yield p
		case EDot(_) => for 
		{
			i <- getIncNGI
		} yield PVar(i,Nil,PE(Any))
		case EAny(cs,_) => for 
		{
			i <- getIncNGI
		} yield PVar(i,Nil,PE(char_list_to_re(cs)))
		case ENoneOf(cs,_) => for 
		{
			i <- getIncNGI
		} yield PVar(i,Nil,PE(Not(cs)))
		case EEscape('s',_) => for  // '\s' -> '\t|\n|\r\ '
		{
			i <- getIncNGI
			tab = PVar(i,Nil,PE(L('\t')))
			nl  = PVar(i,Nil,PE(L('\n')))
			rt  = PVar(i,Nil,PE(L('\r')))
			sp  = PVar(i,Nil,PE(L(' ')))
			g   = Greedy
		} yield PChoice(tab, PChoice(nl, PChoice(rt, sp, g), g), g)
		case EEscape(c,_) => for 
		{
			i <- getIncNGI
		} yield PVar(i,Nil,PE(L(c)))
		case EChar(c,_) => for
		{
			i <- getIncNGI
		} yield PVar(i,Nil,PE(L(c)))
	}

	def r_trans(epat:EPat):State[TState,RE] = epat match 
	{
		case EEmpty(_) => point(Empty)
		case EGroup(e,_) => r_trans(e) // this is not possible
		case EGroupNonMarking(e,_) => r_trans(e) 
		case EOr(es,_) => 
		{
			def mkChoice(rs:List[RE]) : RE = rs match 
			{
				case (r::rs_) => rs_.foldLeft(r)( (r1:RE,r2:RE) => RE.Choice(r1,r2,Greedy))
				case Nil => Phi
			}
			for 
			{
				rs <- es.traverseS(r_trans(_))
			} yield mkChoice(rs)
		}
		case EConcat(es,_) => 
		{
			def mkConcat(rs:List[RE]) : RE = rs match 
			{
				case (r::rs_) => rs_.foldLeft(r)( (r1:RE,r2:RE) => Seq(r1,r2))
				case Nil => Empty
			}
			for 
			{
				rs <- es.traverseS(r_trans(_))
			} yield mkConcat(rs)
		}

		case EOpt(e,b,_) => 
		{
			val g = if (b) { Greedy	} else { NotGreedy }
			for 
			{ 
				r <- r_trans(e) 
			} yield RE.Choice(r,Empty,g)
		}
		case EPlus(e,b,_) => 
		{
			val g = if (b) { Greedy	} else { NotGreedy }
			for 
			{ 
				r <- r_trans(e) 
			} yield Seq(r,Star(r,g))
		}
		case EStar(e,b,_) => 
		{
			val g = if (b) { Greedy	} else { NotGreedy }
			for 
			{ 
				r <- r_trans(e) 
			} yield Star(r,g)
		}
		case EBound(e,low,Some(high),b,_) =>
		{
			val g = if (b) { Greedy	} else { NotGreedy }		
			def go(r:RE):RE = 
			{
				val r1s = Nil.padTo(low,r)
				val r1  = mkSeq(r1s)
				val r2s = Nil.padTo(high - low, RE.Choice(r,Empty,g))
				val r2  = mkSeq(r2s)
				(r1,r2) match 
				{
					case (Empty,Empty) => Empty
					case (Empty,_)     => r2
					case (_    ,Empty) => r1
					case (_    ,_)     => Seq(r1,r2)
				}
			}
			for 
			{
				r <- r_trans(e)
			} yield go(r)			
		}
		case EBound(e,low,None,b,_) => 
		{
			val g = if (b) { Greedy	} else { NotGreedy }
			def go(r:RE):RE =
			{
				val r1s = Nil.padTo(low,r)
				val r1  = mkSeq(r1s)
				Seq(r1,Star(r,g))
			}
			for 
			{
				r <- r_trans(e)
			} yield go(r)	
		}
		case ECarat(_) => for 
		{	// currently we anchor the entire expression regardless of where ^ appears, we turn the subsequent ECarat into literal
			f <- getAnchorStart
			x <- if (f) 
			{
				point(L('^'))
			} else 
			{ 
				for 
				{
					_ <- setAnchorStart
				} yield Empty
			}
		} yield x
		case EDollar(_) => for 
		{	// similar to ecarat, excep that we will not turn treat the subsequent EDollars as literal
			f <- getAnchorEnd
			x <- if (f)
			{
				point(())
			} else 
			{
				setAnchorEnd
			}
		} yield Empty

		case EDot(_) => point(Any)

		case EAny(cs,_) => point(char_list_to_re(cs))

		case ENoneOf(cs,_) => point(Not(cs))

		case EEscape(c,_) => point(L(c))

		case EChar(c,_) => point(L(c))
	}
}

/**
scala> import com.github.luzhuomi.scalazparsec.NonBacktracking._
scala> import com.github.luzhuomi.regex.Parser._
scala> import com.github.luzhuomi.regex.Translate._ 
scala> parseEPat("^[ab]*$") match { case Consumed(Some((x,_))) => translate(x) }
res12: com.github.luzhuomi.regex.IntPattern.Pat = PVar(0,List(),PE(Seq(Seq(Empty,Star(Choice(L(a),L(b),Greedy),Greedy)),Empty)))
*/