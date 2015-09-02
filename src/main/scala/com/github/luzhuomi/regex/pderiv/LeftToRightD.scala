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


}