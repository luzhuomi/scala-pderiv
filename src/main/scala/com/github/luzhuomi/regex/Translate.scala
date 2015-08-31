package com.github.luzhuomi.regex

/**
* Translating from External AST to the Internal AST.
*
*/ 

import scalaz._
import scalaz.{ State => S }
import Scalaz._
import com.github.luzhuomi.regex.ExtPattern._
import com.github.luzhuomi.regex.IntPattern._
import com.github.luzhuomi.regex.RE._
import com.github.luzhuomi.regex.Common._

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
				case ((true,true)) => PVar(mainBinder,Nil,pat)
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

	// a tricky wrapper to have point with a slightly more specific instance of Monad State
	def point[A](a: => A)(implicit m:Monad[({type λ[B] = State[TState,B]})#λ]):State[TState,A] = m.point(a)

	def p_trans(epat:EPat):State[TState,Pat] = epat match 
	{
		// () ~>_p ()
		case EEmpty => point(PE(Empty))
		/**
		* e ~> p
		* ------------------
		* ( e ) ~>_p x :: p
		*/
		case EGroup(e) => for 
		{
			i <- getIncGI
			p <- trans(e)
		} yield PVar(i,Nil,p)
		case EGroupNonMarking(e) => trans(e)
		case EOr(es) => 
		{ 
			def mkChoice(ps:List[Pat]) : Pat = ps match 
			{
				case (p::ps_) => ps_.foldLeft(p)( (p1:Pat,p2:Pat) => PChoice(p1,p2,Greedy))
			}
			for 
			{
				ps <- es.traverseS(trans(_))
			} yield mkChoice(ps)
		}
	}

	def r_trans(epat:EPat):State[TState,RE] = epat match 
	{
		case EEmpty => point(Empty)
	}
}