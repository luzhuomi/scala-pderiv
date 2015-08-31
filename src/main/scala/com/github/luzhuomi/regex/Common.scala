package com.github.luzhuomi.regex

object Common
{
	type Range = (Int,Int)

	sealed trait GFlag 
	case object Greedy extends GFlag
	case object NotGreedy extends GFlag

	val preBinder = -1
	val preBinder_ = -2
	val subBinder = 2147483647
	val mainBinder = 0

}