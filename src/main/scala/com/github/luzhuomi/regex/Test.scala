package com.github.luzhuomi.regex


import scalaz._
import Scalaz._
import com.github.luzhuomi.scalazparsec.NonBacktracking._
// import com.github.luzhuomi.scalazparsec.Parsec._


object Test
{

	
	def test(x:String):Result[Option[(List[Token], List[Token])]] =
	{
		def m:Parser[List[Token]] = for
		{
			xs <- many(digit)
		} yield xs
		run(m)(x.toList)
	}
	/*
	def test(x:String):Option[(List[Token], List[Token])] =
	{
		def m:Parser[List[Token]] = for
		{
			xs <- many(digit)
		} yield xs
		run(m)(x.toList)
	}
	*/
	def digit:Parser[Char] = sat (_.isDigit)

}