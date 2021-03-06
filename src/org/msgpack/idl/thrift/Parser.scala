package org.msgpack.idl.thrift

import util.parsing.combinator.syntactical.StdTokenParsers
import util.parsing.combinator.ImplicitConversions
import org.msgpack.idl.ast._
import scala.collection.mutable.HashMap


class Parser  extends StdTokenParsers with ImplicitConversions {
	type Tokens = Lexer
	val lexical = new Tokens
	import lexical.NumericLit

	private val symbolTable = new HashMap[String, NodeType]

	// FIXME: replace StdLexer.Keyword to original tokens
	lexical.reserved ++= List(
		"struct", "namespace", "required", "optional",
		"string", "i8", "16", "i32", "i64", "double", "list"
	)
	lexical.delimiters ++= List("\n", "\r", " ", "\t")

	// Create token to TypeClass map
	// FIXME: replace StdLexer.Keyword to original tokens
	Array[(String, NodeType)](
		("i8", TypeInt8()),
		("i16", TypeInt16()),
		("i32", TypeInt32()),
		("i64", TypeInt64()),
		("bool", TypeBoolean()),
		("double", TypeDouble()),
		("string", TypeString())
	).foreach(symbolTable += _)

	def idl =  rep(idl_nodes) ^^ { case nodes => ThriftIDL(nodes) }
	def idl_nodes = (namespace | struct) ^^ {
		case (idl: IDLDefinition) =>
			idl
	}

	def namespace: Parser[Any] = "namespace" ~ ident ~ ident ^^ {
		case _ ~ lang ~ space =>
			IDLNamespace(lang, space)
	}

	def struct: Parser[Any] =
		("struct" ~ (ident ^^ {case s => symbolTable.put(s, TypeSymbol(s)); s })
					~ "{" ~ rep(member) ~ "}" ) ^^ {
		case name ~ _ ~ (members: List[StructMember]) ~ _ =>
			IDLStruct(name._2, members)
	}

	def member: Parser[Any] = lNumber ~ ":" ~ required ~ typ ~ ident ~ ";" ^^ {
		case num ~ (req: Boolean) ~ (typ: NodeType) ~ name ~ _ =>
			StructMember(num._1, name, typ, req)
	}

	def lNumber = accept("number", { case NumericLit(n) => n.toInt })

	def required: Parser[Any] =
		"required" ^^ { case _ => true } |
		"optional" ^^ { case _ => false }

	def typ: Parser[Any] = (
			("i8" | "i16" | "i32" | "i64" | "double" | "string") ^^ {
				case s: String =>
					types(s)
			} |
			list |
			ident ^^ { case s => types(s) }
			)

	def list: Parser[Any] = ("list" ~ "<" ~ ident ~ ">") ^^ {
		case _ ~ _ ~ indent ~ _ =>
			TypeList(types(indent))
	}

	private def types(s: String): NodeType = {
		symbolTable.get(s) match {
			case Some(t) => t
			case None =>
				throw new Exception("Symbol not found " + s)
		}
	}

	def parse(input: String) {
		val token = new lexical.Scanner(input)
		phrase(idl)(token) match {
			case Success(idlNode, _) =>
				println("== definitions")
				idlNode.defs.foreach(p => println(p))
				println("== registered symbols")
				symbolTable.foreach(p => println(p))
			case e =>
				error(e.toString)
		}
	}
}

