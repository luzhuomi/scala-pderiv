package com.github.luzhuomi.regex


import scalaz._
import Scalaz._
import com.github.luzhuomi.scalazparsec.NonBacktracking._
import com.github.luzhuomi.regex.ExtPattern._
import com.github.luzhuomi.regex.IntPattern._
import com.github.luzhuomi.regex.RE._

object Parser 
{

	def parseEPat(x:String):Result[Option[(EPat,List[Token])]] = 
	{
		def m:Parser[EPat] = for 
		{
			pat <- p_ere
			_   <- eof
		} yield (pat)
		run(m)(x.toList)
	}


	def getLoc:Parser[Int] = for 
	{
		tokens <- getState
	} yield tokens.length

	// same as token from scalazparsec 0.1.1
	def char(x:Token):Parser[Token] = sat( ((y:Token) => y == x) )

	def anyChar:Parser[Token] = item

	def oneOf(s:String):Parser[Token] = 
	{
		val l = s.toSet
		sat(l.contains(_))
	}

	def noneOf(s:String):Parser[Token] = 
	{
		val l = s.toSet
		sat(!l.contains(_))
	}
	

	def eof:Parser[Unit] = for 
	{
		x <- notFollowedBy(item)
	} yield(x)

	def notFollowedBy[A](p:Parser[A]):Parser[Unit] = 
	{
		def t:Parser[Unit] = for 
		{
			c <- attempt(p)
			x <- (unexpected:Parser[Unit])
		} yield(x)
		def t2:Parser[Unit] = point(())
		attempt(+++(t)(t2))
	}

	def unexpected[A](implicit m:MonadPlus[Parser]): Parser[A] = m.empty

	def between[A,B,C](p1:Parser[A],p2:Parser[B],p3:Parser[C]) :Parser[C] = for 
	{
		_ <- p1
		x <- p3 
		_ <- p2
	} yield x

	def option[A](a:A,p:Parser[A]) = 
		+++(p)(point(a))

	def when(cond:Boolean)(p:Parser[Unit]):Parser[Unit] = 
	{
		if (cond) 
		{ 
			p 
		} 
		{ 
			point(())
		}
	}

	def guard(cond:Boolean)(implicit m:MonadPlus[Parser]):Parser[Unit] = 
	{
		if (cond)
		{ point(()) }
		else 
		{ m.empty }
	}


	def string(s:String):Parser[String] = 
	{
		def string_(l:List[Char]):Parser[List[Char]] = l match 
		{
			case Nil => point(Nil)
			case (c::cs) => for
			{
				_ <- sat (_ == c)
				_ <- string_(cs)
			} yield (c::cs)
		}

		for 
		{
			l <- string_(s.toList)
		} yield l.mkString
	}



	def digit:Parser[Char] = sat (_.isDigit)

	def p_ere : Parser[EPat] = for 
	{
		sLoc <- getLoc
		ps <- interleave(p_branch)(char('|'))
		eLoc <- getLoc
		loc = (sLoc,eLoc)
	} yield EOr(ps,loc)

	def p_branch : Parser[EPat] = for 
	{
		sLoc <- getLoc
		ps <- many1(p_exp)
		eLoc <- getLoc
		loc = (sLoc,eLoc)
	} yield EConcat(ps,loc)
	
	def p_exp : Parser[EPat] = for 
	{
		atom <- +++ (p_anchor) (p_atom)
		atom2 <- p_post_anchor_or_atom(atom)
	} yield atom2

	def p_anchor : Parser[EPat] = 
	{ 
		def p1:Parser[EPat] = for 
		{
			sLoc <- getLoc
			c <- char('^')
			eLoc <- getLoc
			loc = (sLoc,eLoc)

		} yield ECarat(loc)
		def p2:Parser[EPat] = for 
		{ 
			sLoc <- getLoc
			c <- char('$')
			eLoc <- getLoc
			loc = (sLoc,eLoc)

		} yield EDollar(loc)
		+++ (p1) (p2)
	}

	def p_atom : Parser[EPat] = 
	+++ (p_group) (+++ (p_charclass) (+++ (p_dot) (+++ (p_esc_char) (p_char))))

	def p_group : Parser[EPat] = 
	{
		def p1:Parser[EPat] = for 
		{
			sLoc <- getLoc
			qn <- char('?')
			co <- char(':')
			eLoc <- getLoc
			x <- p_ere
			loc = (sLoc,eLoc)
		} yield EGroupNonMarking(x,loc)

		def p2:Parser[EPat] = for 
		{
			sLoc <- getLoc
			x <- p_ere
			eLoc <- getLoc
			loc = (sLoc,eLoc)
		} yield EGroup(x,loc)

		def p3:Parser[EPat] = +++ (attempt(p1)) (p2)

		between(char('('),char(')'),p3)
	}

	def p_charclass:Parser[EPat] = 
	{
		def pnone:Parser[EPat] = for
		{
			sLoc <- getLoc
			_ <- char('^')
			enum <- p_enum  // enum ends with ']'
			eLoc <- getLoc
			loc = (sLoc,eLoc)
		} yield ENoneOf(enum,loc)
		def pany:Parser[EPat] = for
		{
			sLoc <- getLoc
			enum <- p_enum
			eLoc <- getLoc
			loc = (sLoc,eLoc)
		} yield EAny(enum,loc)
		for 
		{
			_ <- char('[')
			x <- +++ (pnone) (pany)
		} yield x
	}

	def p_enum : Parser[List[Char]] = 
	{
		def rsb:Parser[List[Char]] = for 
		{
			_ <- char(']')
		} yield(List(']'))

		def dash:Parser[List[Char]] = for 
		{
			_ <- char('-')
		} yield(List('-'))
		for 
		{
			initial <- option(Nil, (+++(rsb)(dash)))
			chars   <- many1(p_one_enum)
			_       <- char(']')
			val chars_set = initial ++ chars.foldLeft(Nil:List[Char])((s,c) => s ++ c)
		} yield (chars_set)
	}

	def p_one_enum : Parser[List[Char]] = +++ (p_range) (p_char_set)

	def p_range : Parser[List[Char]] =  
	{
		def p:Parser[List[Char]] = for 
		{
			start <- +++(attempt(p_esc_char_))(noneOf("]-"))
			_     <- char('-')
			end   <- +++(attempt(p_esc_char_))(noneOf("]"))
		} yield((start to end).toList)

		attempt(p)
	}

	def p_char_set:Parser[List[Char]] = 
	{
		def isEnd:Parser[Boolean] = 
		{ 
			def p1:Parser[Boolean] = for 
			{
				_ <- lookAhead(char(']'))
			} yield (true) 
			+++(p1)(point(false))
		}

		def checkEnd:Parser[Unit] = for 
		{
			atEnd <- isEnd
			_ <- when (!atEnd)(unexpected)
		} yield ()
		for 
		{
			c <- +++ (attempt(p_esc_char_)) (noneOf("]"))
			_ <- when(c=='-')(checkEnd)
		} yield (List(c))
	}

	def p_dot:Parser[EPat] = for 
	{
		sLoc <- getLoc
		_ <- char('.')
		eLoc <- getLoc
		loc = (sLoc,eLoc)
	} yield EDot(loc)

	def p_esc_char_ : Parser[Token] = for 
	{
		_ <- char('\\')
		x <- +++(attempt(p_tab))(+++ (attempt(p_return)) (+++ (attempt(p_newline)) (+++ (attempt(p_oct_ascii)) (anyChar))))
	} yield (x)

	def p_esc_char : Parser[EPat] = for 
	{
		sLoc <- getLoc
		c <- p_esc_char_
		eLoc <- getLoc
		loc = (sLoc,eLoc)
	} yield EEscape(c,loc)

	def p_return : Parser[Char] = for 
	{
		_ <- char('r')
	} yield ('\r')

	def p_newline : Parser[Char] = for
	{
		_ <- char('n')
	} yield ('\n')

	def p_tab : Parser[Char] = for
	{
		_ <- char('t')
	} yield ('\t')

	def p_oct_ascii : Parser[Char] = for 
	{
		d1 <- digit
		d2 <- digit
		d3 <- digit
	} yield (d2.asDigit * 8 + d3.asDigit).toChar

	val specials = "^.[$()|*+?{\\"

	def p_char : Parser[EPat] = for 
	{
		sLoc <- getLoc
		c <- noneOf(specials)
		eLoc <- getLoc
		loc = (sLoc,eLoc)
	} yield EChar(c,loc)

	def p_special_char : Parser[Token] = oneOf(specials)

	def p_post_anchor_or_atom(atom:EPat):Parser[EPat] = 
	{
		def optng : Parser[EPat] = for 
		{
			sLoc <- getLoc
			_ <- char('?')
			_ <- char('?')
			eLoc <- getLoc
			loc = (sLoc,eLoc)
		} yield EOpt(atom,false,loc)

		def optg : Parser[EPat] = for
		{
			sLoc <- getLoc
			_ <- char('?')
			eLoc <- getLoc
			loc = (sLoc,eLoc)
		} yield EOpt(atom,true,loc)
		
		def plusng : Parser[EPat] = for 
		{
			sLoc <- getLoc
			_ <- char('+')
			_ <- char('?')
			eLoc <- getLoc
			loc = (sLoc,eLoc)
		} yield EPlus(atom,false,loc)
		
		def plusg : Parser[EPat] = for
		{
			sLoc <- getLoc
			_ <- char('+')
			eLoc <- getLoc
			loc = (sLoc,eLoc)
		} yield EPlus(atom,true,loc)
		
		def starng : Parser[EPat] = for 
		{
			sLoc <- getLoc
			_ <- char('*')
			_ <- char('?')
			eLoc <- getLoc
			loc = (sLoc,eLoc)
		} yield EStar(atom,false,loc)
		
		def starg : Parser[EPat] = for
		{
			sLoc <- getLoc
			_ <- char('*')
			eLoc <- getLoc
			loc = (sLoc,eLoc)
		} yield EStar(atom,true,loc)

		val opt = +++(attempt(optng))(optg) 
		val plus = +++(attempt(plusng))(plusg)
		val star = +++(attempt(starng))(starg)
		+++(opt)(+++(plus)(+++(star)(+++(p_bound_nongreedy(atom))(+++(p_bound(atom))(point(atom))))))
	}

	def p_bound_nongreedy(atom:EPat):Parser[EPat] = 
		attempt(between(char('{'),string("}?"),p_bound_spec(atom,false)))

	def p_bound(atom:EPat):Parser[EPat] = 
		attempt(between(char('{'),char('}'),p_bound_spec(atom,true)))

	def p_bound_spec(atom:EPat,b:Boolean):Parser[EPat] = 
	{
		def readHighS:Parser[List[Token]] = for
		{
			_ <- char(',')
			highS <- many(digit)
		} yield highS

		def mkHighI(lowI:Int,highS:List[Token]):Parser[Option[Int]] = 
		{
			if (highS.size == 0)
			{
				point(None)
			}
			else 
			{
				val highI = highS.mkString.toInt
				for 
				{
					_ <- guard (lowI <= highI)
				} yield (Some(highI))
			}
		} 

		def p(lowI:Int):Parser[Option[Int]] = for 
		{
			h <- readHighS
			x <- mkHighI(lowI,h)
		} yield x

		for 
		{
			sLoc <- getLoc
			lowS <- many1(digit)
			val lowI = lowS.mkString.toInt
			highMI <- option(Some(lowI),attempt(p(lowI)))
			eLoc <- getLoc
			loc = (sLoc,eLoc)
		} yield (EBound(atom,lowI,highMI,b,loc))
	}

}
