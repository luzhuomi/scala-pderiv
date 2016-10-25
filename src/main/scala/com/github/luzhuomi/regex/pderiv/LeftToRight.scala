package com.github.luzhuomi.regex.pderiv


import scala.collection.immutable.IntMap
import scala.collection.immutable.Map
import com.github.luzhuomi.regex.pderiv.IntPattern._
import com.github.luzhuomi.regex.pderiv.RE._
import com.github.luzhuomi.regex.pderiv.Common._
object LeftToRight
{
	type Word = String



	type Env  = List[(Int,Word)]

	def rg_collect(w:Word, r:(Int,Int)):Word = r match
	{
		case ((i,j)) => w.drop(i).take(j - i + 1)
	}

	type MatchNFA = IntMap[List[(Int,Int=>Binder=>Binder)]]
	// not needed? // NFA table
	def buildMatchNFA(init:Pat) : (MatchNFA, List[Int]) =
	{
		val sig       = sigmaRE(strip(init)) map (x => (x,0))
		val init_dict = Map.empty + (init -> (init,0))
		val (all,delta,dictionary) = builder(sig,Nil,Nil,List(init),init_dict,1)
		val finals    = all filter ( (p:Pat) => (implicitly[PosEpsilon[RE]].posEpsilon(strip(p))) )
		val sfinals   = finals map (mapping(dictionary,_))
		val lists     = for
		{
			(p,l,qfs) <- delta
			val i = mapping(dictionary,p)
			val jfs = qfs map ( qf => (mapping(dictionary,qf._1),qf._2))
		} yield (i,l,jfs)
		val em:MatchNFA = IntMap.empty
		val hash_table = lists.foldLeft(em)( (dict,pxq) =>
		{
			val (p,x,q) = pxq
			val k = my_hash(p,x._1)
			dict.get(k) match
			{
				case Some(ps) => // should signal an error
					{ dict }
				case None => dict + (k -> q)
			}
		})
		(hash_table,sfinals)
	}
	// mapping pat to unique Int
	def mapping(dict:Map[Pat,(Pat,Int)], p:Pat) : Int = dict.get(p) match
	{
		case Some(x) => x._2
		case None    => -1
		// None is impossible
	}

	def builder(sig:List[Letter],
		acc_states:List[Pat],
		acc_delta:List[(Pat,Letter,List[(Pat,Int=>Binder=>Binder)])],
		curr_states:List[Pat],
		dict:Map[Pat,(Pat,Int)],
		max_id:Int
		):(List[Pat], List[(Pat,Letter,List[(Pat,Int => Binder => Binder)])], Map[Pat,(Pat,Int)]) =
	{
		if (curr_states.isEmpty)
		{
			(acc_states,acc_delta,dict)
		} else
		{
			val all_sofar_states = acc_states ++ curr_states
			val new_delta = for
			{
				s <- curr_states
				l <- sig
				val sfs = pdPat0Sim(s,l)
			} yield (s,l,sfs)
			val new_states = (for
			{
				(_,_,sfs) <- new_delta
				(s,f) <- sfs
				if (!(dict.contains(s)))
			} yield s).toSet.toList
			val acc_delta_next = acc_delta ++ new_delta
			val (dictp,max_idp) = new_states.foldLeft((dict,max_id))( (did:(Map[Pat,(Pat,Int)],Int),p:Pat) => (did._1 + (p -> (p,did._2)), did._2+1) )
			builder(sig,all_sofar_states,acc_delta_next,new_states,dictp,max_idp)
		}
	}

	type NFAStates = List[Int]

	

	def lookupTransit(nfa:MatchNFA, currState:Int, l:Letter) : List[(Int, (Int, Int => Binder => Binder))] =
	{
		val k = my_hash(currState,l._1)
		nfa.get(k) match
		{
			case Some(pairs) => pairs.map ( jop =>
			{
				val (nextState,op) = jop
				(nextState, (currState, op))
			})
			case None => Nil
		}
	}

	def collectPatMatchFromBinder(w:Word,b:Binder):Env = collectPatMatchFromBinder_(w,listify(b))

	def collectPatMatchFromBinder_(w:Word,l:List[(Int,List[Range])]):Env = l match
	{
		case Nil           => Nil
		case ((x,Nil)::xs) => (x,"")::(collectPatMatchFromBinder_(w,xs))
		case ((x,rs)::xs)  =>
		{
			val rg = rs.reverse.map( rg_collect(w,_) ).mkString("")
			(x,rg)::(collectPatMatchFromBinder_(w,xs))
		}
	}


	// using function composition seem causing a stack over flow. rolling back to just computing the List[binder]
	def patMatchesIntStatePdPat(cnt:Int,
		nfa:MatchNFA,
		word:Word,
		currNfaStateBinders:List[(Int,Binder)]
		):List[(Int,Binder)] = currNfaStateBinders match
	{
		case Nil => Nil
		case (x::xs) =>
		{
			if (word.isEmpty) { currNfaStateBinders }
			else
			{
				val l   = word.head
				val w   = word.tail
				val nextNfaStateBinders = currNfaStateBinders flatMap ( x => x match { case (nfaState,binder) => 
				{
					val transitions:List[(Int,(Int, Int => Binder => Binder))] = lookupTransit(nfa,nfaState,(l,cnt))
					transitions map ( x => 
						{ 
							val nextState = x._1
							val currState = x._2._1
							val g = x._2._2 
							(nextState, g(cnt)(binder))
						})
				}} )
				val cnt_ = cnt + 1
				patMatchesIntStatePdPat(cnt_,nfa,w,nub2(nextNfaStateBinders))
				
			}
		}
	}




	def greedyPatMatch(p:Pat,w:Word):Option[Env] = buildMatchNFA(p) match
	{
		case (nfa,sfinal) =>
		{
			val s = 0 // start nfa state
			val b = toBinder(p)
			// def id(x:Binder) = x
			val states_and_binders = patMatchesIntStatePdPat(0,nfa,w,List((s,b)))
			val binders  = states_and_binders.filter((t:(Int,Binder)) => sfinal.contains(t._1) ).map ( t => t._2 )
			binders match
			{
				case Nil    => None
				case (b::_) => Some(collectPatMatchFromBinder(w,b))
			}

		}
	}

	type CompiledPat = (MatchNFA,List[Int],Binder)

	def compilePat(p:Pat):CompiledPat = buildMatchNFA(p) match
	{
		case (nfa,sfinal) =>
		{
			val b = toBinder(p)
			(nfa,sfinal,b)
		}
	}

	def greedyPatMatchCompiled(compiled:CompiledPat,w:Word):Option[Env] = compiled match
	{
		case (nfa,sfinal,b) =>
		{
			val s = 0 // start nfa state
			val states_and_binders = patMatchesIntStatePdPat(0,nfa,w,List((s,b)))
			val binders  = states_and_binders.filter((t:(Int,Binder)) => sfinal.contains(t._1) ).map ( t => t._2 )
			binders match
			{
				case Nil    => None
				case (b::_) => Some(collectPatMatchFromBinder(w,b))
			}
		}
	}

}
