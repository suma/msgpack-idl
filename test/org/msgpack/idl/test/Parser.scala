package org.msgpack.idl.test


object Parser {
	def main(args: Array[String]) = {
		val input = """
namespace java my.first.idl
namespace cpp my.first.idl

struct Column {
    1: required string name;
    2: required string value;
    3: required i64 timestamp;
    4: optional i32 ttl;
}

struct SuperColumn {
    1: required string name;
    2: required list<Column> columns;
}

"""
		println("input = " + input)
//		println(parseAll(idl, input))
		val a = new org.msgpack.idl.thrift.Parser
		a.parse(input)
	}
}