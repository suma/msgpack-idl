package org.msgpack.idl.thrift

import org.msgpack.idl.ast._

case class TypeList(typ: NodeType) extends NodeType

case class NodeElement(index: Int, name: String, typ: NodeType, required: Boolean)

case class IDLDefinition
case class IDLStruct(name: String, member: List[NodeElement]) extends IDLDefinition
case class IDLNamespace(lang: String, space: String) extends IDLDefinition

case class ThriftIDL(defs: List[IDLDefinition])





