package org.msgpack.idl.thrift

import scala.util.logging.ConsoleLogger
import scala.collection.mutable.{Stack, ArrayBuffer, HashMap}
import org.msgpack.idl.ast._

case class TypeList(typ: NodeType) extends NodeType

case class NodeElement(index: Int, name: String, typ: NodeType, required: Boolean)
case class NodeStruct(name: String, member: List[NodeElement])

case class IDLStruct(node: NodeStruct)
case class IDLNamespace(name: String, space: String)

case class ThriftIDL(nodes: List[ThriftIDL])





