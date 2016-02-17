package com.github.luzhuomi.regex.pderiv


import scala.collection.immutable.IntMap
import scala.collection.immutable.Map
import com.github.luzhuomi.regex.IntPattern._
import com.github.luzhuomi.regex.RE._
import com.github.luzhuomi.regex.Common._
object LeftToRightD 
{
	type Word = String



	type Env  = List[(Int,Word)]

	def rg_collect(w:Word, r:(Int,Int)):Word = r match 
	{
		case ((i,j)) => w.drop(i).take(j - i + 1)
	}

	type PDPat0Table = IntMap[List[(Int,Int=>Binder=>Binder)]]
	// not needed? // NFA table
	def buildPd0Table(init:Pat) : (PDPat0Table, List[Int]) = 
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
		val em:PDPat0Table = IntMap.empty
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

	type DPat0Table = IntMap[(Int,NFAStates,IntMap[List[Int=>Binder=>Binder]])]
	// building DFA table
	def buildDPat0Table(pat:Pat):(DPat0Table,List[Int]) = 
	{
		val sig       = sigmaRE(strip(pat)) map (x => (x,0)) 
		val (hash_table,sfinals) = buildPd0Table(pat)
		// building DFA
		val init = List(0)
		val init_dict = Map.empty + (init -> 0)
		val (all,delta,dictionary) = builder_(hash_table,sig,Nil,Nil,List(init), init_dict, 1)
		val list = delta.map( clnf => 
		{
			clnf match 
			{
				case ((c,l,n,f)) => 
				{
					val i = mapping_(dictionary)(c)
					val j = mapping_(dictionary)(n)
					(i,l,j,n,f)
				}
			}
		})
		val em:DPat0Table = IntMap.empty
		val hash_table_ = list.foldLeft(em)( (dict,iljnf) => iljnf match
		{
			case ((i,l,j,n,f)) => 
			{
				val k = my_hash(i,l._1)
				dict.get(k) match 
				{
					case Some(ps) => // should signal an error
					{ dict }
					case None => dict + (k,(j,n,f))
				}
			}
		})
		(hash_table_,sfinals)
	}

	def mapping_(dict:Map[NFAStates,Int])(p:NFAStates) = dict.get(p) match
	{
		case Some(x) => x
		case None    => -1 // None is impossible
	}

	def builder_(pdStateTable:PDPat0Table, 
		sig:List[Letter],
		acc_states:List[NFAStates],
		acc_delta:List[(NFAStates,Letter, NFAStates, IntMap[List[Int=>Binder=>Binder]])],
		curr_states:List[NFAStates],
		dict:Map[NFAStates,Int],
		max_id:Int
		):(List[NFAStates], List[(NFAStates,Letter, NFAStates,IntMap[List[Int=>Binder=>Binder]])], Map[NFAStates,Int]) = curr_states match 
	{
		case Nil => (acc_states, acc_delta, dict)
		case curr_states => 
		{
			val all_sofar_states = acc_states ++ curr_states
			def insert(k:Int,f:Int=>Binder=>Binder,im:IntMap[List[Int=>Binder=>Binder]]):IntMap[List[Int=>Binder=>Binder]] = im.get(k) match 
			{
				case Some(fs) => im.updated(k,fs++List(f))
				case None     => im + (k -> List(f))
			}
			val new_delta = curr_states.flatMap ( curr_state => 
			{
				sig.map( l => 
				{
					val pairs = nub2(curr_state.flatMap( nfastate => lookupPdPat1(pdStateTable,nfastate,l)))
					val (next_state, curr_state_and_f_pairs) = pairs.unzip
					val em:IntMap[List[Int=>Binder=>Binder]] = IntMap.empty
					val f_dict = curr_state_and_f_pairs.foldLeft(em)((d,lf) => 
					{
						val (l,f) = lf
						insert(l,f,d)
					})
					(curr_state, l, next_state, f_dict)
				})
			})
			val new_states = (for 
			{
				(_,_,next_state,_) <- new_delta
				if (!dict.contains(next_state))
			} yield next_state).distinct
			val acc_delta_next = acc_delta ++ new_delta
			val (dictp,max_idp) = new_states.foldLeft((dict,max_id))( (did,p) => 
			{
				val (d,max_id) = did
				val dp = d + (p -> max_id)
				val max_idp = max_id + 1
				(dp,max_idp)
			})
			builder_(pdStateTable, sig, all_sofar_states, acc_delta_next, new_states, dictp, max_idp)
		}
	}

	def lookupPdPat1(hash_table:PDPat0Table, i:Int, l:Letter) : List[(Int, (Int, Int => Binder => Binder))] = 
	{
		val k = my_hash(i,l._1)
		hash_table.get(k) match 
		{
			case Some(pairs) => pairs.map ( jop => 
			{
				val (j,op) = jop
				(j, (i, op))
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

	// OPTIMIZATION: instead of computing List[Binder], we keep track of a List[Binder=>Binder] (binder update functions compositions)
	//               the composition is only applied to the binder when the path is yieding a final state. (speed up 14s -> 10s on USAddress)
	// OPTIMIZATION #2: factor out the dfsState from the List of (DFAState,NFAState,BinderFunction)s, (speed up 10s -> 8.7s on USAddress)
	def patMatchesIntStatePdPat(cnt:Int,
		dStateTable:DPat0Table,
		word:Word,
		currDfaState:Int,
		currNfaStateBinders:List[(Int,Binder=>Binder)]
		):(Int,List[(Int,Binder=>Binder)]) = (currDfaState,currNfaStateBinders) match 
	{
		case (i,Nil) => (i,Nil)
		case (i,(x::xs)) => 
		{
			if (word.isEmpty) { (i,currNfaStateBinders) } 
			else 
			{
				val l            = word.head
				val w            = word.tail
				val k            = my_hash(i,l)
				dStateTable.get(k) match 
				{
					case None                           => (i,Nil) 
					case Some((j,next_nfaStates,fDict)) => // j is the next dfs state
					{
						def go(a:List[Binder=>Binder], smb:(Int,Binder=>Binder)):List[Binder=>Binder] = 
						{
							val (m,bf) = smb
							fDict.get(m) match 
							{
								case None     => a
								case Some(gs) => a ++ gs.map (g => (g(cnt) compose bf))
							}
						}
						val em:List[Binder=>Binder] = Nil
						val binderfuncs:List[Binder=>Binder] = currNfaStateBinders.foldLeft(em)(go)
						val nextNfaStateBinders  = next_nfaStates.zip(binderfuncs)
						val cnt_ 				 = cnt + 1
						patMatchesIntStatePdPat(cnt_,dStateTable,w,j,nextNfaStateBinders)
					}
				}
			}
		}
	}
	
	def greedyPatMatch(p:Pat,w:Word):Option[Env] = buildDPat0Table(p) match 
	{
		case (dStateTable,sfinal) => 
		{
			val s = 0 // start nfa state
			val t = 0 // start dfa state
			val b = toBinder(p)
			def id(x:Binder) = x
			val (_, allbinders_) = patMatchesIntStatePdPat(0,dStateTable,w,t,List((s,id)))
			val allbinders  = allbinders_.filter( t => sfinal.contains(t._1) ).map ( t => t._2 )
			allbinders match 
			{
				case Nil    => None
				case (f::_) => Some(collectPatMatchFromBinder(w,f(b)))
			}
			
		}
	}

	type CompiledPat = (DPat0Table,List[Int],Binder)

	def compilePat(p:Pat):CompiledPat = buildDPat0Table(p) match 
	{
		case (dStateTable,sfinal) => 
		{
			val b = toBinder(p)
			(dStateTable,sfinal,b)
		}
	}

	def greedyPatMatchCompiled(compiled:CompiledPat,w:Word):Option[Env] = compiled match 
	{
		case (dStateTable,sfinal,b) => 
		{
			val s = 0 // start nfa state
			val t = 0 // start dfa state
			def id(x:Binder) = x
			val (_, allbinders_) = patMatchesIntStatePdPat(0,dStateTable,w,t,List((s,id)))
			val allbinders  = allbinders_.filter( t => sfinal.contains(t._1) ).map ( t => t._2 )
			allbinders match 
			{
				case Nil    => None
				case (f::_) => Some(collectPatMatchFromBinder(w,f(b)))
			}
			
		}
	}

}
