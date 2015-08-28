package com.github.luzhuomi.regex

object Common
{
	type Range = (Int,Int)

	sealed trait GFlag 
	case object Greedy extends GFlag
	case object NotGreedy extends GFlag


}