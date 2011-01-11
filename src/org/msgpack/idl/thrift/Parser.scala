package org.msgpack.idl.thrift

import util.parsing.combinator.syntactical.StdTokenParsers
import util.parsing.combinator.{JavaTokenParsers, ImplicitConversions}
import org.msgpack.idl.ast._
import scala.collection.mutable.HashMap


class Parser  extends StdTokenParsers with ImplicitConversions {
	type Tokens = Lexer
	val lexical = new Tokens
	import lexical.{NumericLit, Keyword, Identifier}

	lexical.reserved ++= List(":", ";", "{", "}", "<", ">",
		"struct", "namespace", "required", "optional",
		"string", "i8", "i32", "i64", "double", "list"
	)
	lexical.delimiters ++= List("\n", "\r", " ", "\t")

	private val typeConv = Array[(String, NodeType)](
		("i8", TypeInt8()),
		("i16", TypeInt16()),
		("i32", TypeInt32()),
		("i64", TypeInt64()),
		("double", TypeDouble()),
		("string", TypeString())
	)
	private val envMap = new HashMap[String, NodeType]
	typeConv.foreach(envMap += _)

	def idl =  rep(idl_nodes) // ^^ { case nodes => ThriftIDL(nodes) }
	def idl_nodes = (namespace | struct)

	def namespace: Parser[Any] = "namespace" ~ ident ~ ident

	def struct: Parser[Any] = ("struct" ~
					(ident ^^ {case s => envMap.put(s, TypeSymbol(s))})
					~ "{" ~ rep(member) ~ "}")

	def member: Parser[Any] = numericLit ~ ":" ~ optional ~ typ ~ ident ~ ";"

	def optional: Parser[Any] = ("optional" | "required")

	def typ: Parser[Any] = (
			("i8" | "i16" | "i32" | "i64" | "double" | "string") ^^ {
				case s: String =>
					println("dbg type = " + s)
					types(s)
			} |
			list | ident
			)

	def list: Parser[Any] = ("list" ~ "<" ~ ident ~ ">") ^^ {
		case _ ~ _ ~ indent ~ _ =>
			println("hogehoge")
			println("dbug " + indent)
			TypeList(types(indent))
	}

	private def types(s: String): NodeType = {
		envMap.get(s) match {
			case Some(t) => t
			case None =>
				throw new Exception("Symbol not found " + s)
		}
	}

	def parse(input: String) {
		val tokens = new lexical.Scanner(input)
		phrase(idl)(tokens) match {
			case Success(idlNode, _) =>
				//initTop
				println(idlNode)
				envMap.foreach(p => println(p))
			case e =>
				error(e.toString)
		}
	}

}

/*
class Parser extends JavaTokenParsers {
	//def value: Parser[Any] = decimalNumber | "struct" | "required" | "optional"
	def idl: Parser[Any] = rep(namespace | struct)

	def namespace: Parser[Any] = ident~ident
	def struct: Parser[Any] = "struct" ~ ident ~ "{" ~ repsep(member, ";") ~ "}"
	def member: Parser[Any] = wholeNumber ~ ":" ~ optional ~ typ ~ ident

	def optional: Parser[Any] = "optional" | "required"

	def typ: Parser[Any] =
		"list"~"<" ~ ident ~ ">" |
		"i32" | "i64" | "double" | "string"


}
*/
