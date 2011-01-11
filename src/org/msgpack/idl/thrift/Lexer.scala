package org.msgpack.idl.thrift

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.token.Tokens


class Lexer extends StdLexical with ImplicitConversions {
	override def token: Parser[Token] =
	( number ^^ NumericLit
	| EofCh ^^^ EOF
	| chrSymbol ^^ addSymbol
	| delim
	| rep(chrAny) ^^ checkKeyword
	| failure("Illegal character")
	)

	def number = zero | (nonzero ~ rep(digit) ^^ { case x ~ y => mkS(x :: y) })

	delimiters ++= List(">")

	def addSymbol(xs: Elem) = {
		Keyword(xs.toString)
	}

	def checkKeyword(xs: List[Any]) = {
		val strRep = mkS(xs)
		if (reserved contains strRep) {
			Keyword(strRep)
		} else if (identifierRe.findFirstIn(strRep) != None) {
			Identifier(strRep)
		} else {
			ErrorToken("Not a Identifier: " + strRep)
		}
	}


	override def whitespace: Parser[Any] =
		rep(whitespaceChar | '!' ~ rep(chrExcept(EofCh, '\n')))

	def nonzero = elem("nonzero digit", d => d.isDigit && d != '0')

	def zero: Parser[String] = '0' ^^^ "0"

	def identifierRe =  """^(\w|[^"])+$""".r

	def chrSymbol: Parser[Elem] = Parser[Elem]('<') | '>' | '{' | '}' | ':' | ';'
	def chrAny = chrExcept(EofCh, ' ', '\n', '\t', '\r', '\"', '!', '>', '<', '{', '}', ':', ';')

	def mkS[A](seq: Seq[A]) = {
		seq mkString ""
	}
}



