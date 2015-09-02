package com.github.luzhuomi.regex

object Common
{
	type Range = (Int,Int)

	type Letter = (Char,Int)

	sealed trait GFlag 
	case object Greedy extends GFlag
	case object NotGreedy extends GFlag

	val preBinder = -1
	val preBinder_ = -2
	val subBinder = 2147483647
	val mainBinder = 0

	trait PosEpsilon[T] 
	{
		def posEpsilon(a:T):Boolean
	}

	trait IsEpsilon[T]
	{
		def isEpsilon(a:T):Boolean
	}

	trait IsPhi[T]
	{
		def isPhi(a:T):Boolean
	}

	trait Simplifiable[T] 
	{
		def simplify(t:T):T
	}

	trait IsGreedy[T]
	{
		def isGreedy(t:T):Boolean
	}

	def my_hash(i:Int, x:Char):Int = x.toInt + 256 * i
}