package org.msgpack.idl.thrift

import org.msgpack.idl.ast._

case class TypeList(typ: NodeType) extends NodeType

case class StructMember(index: Int, name: String, typ: NodeType, required: Boolean)

case class IDLDefinition
case class IDLStruct(name: String, member: List[StructMember]) extends IDLDefinition
case class IDLNamespace(lang: String, space: String) extends IDLDefinition

case class ThriftIDL(defs: List[IDLDefinition])





