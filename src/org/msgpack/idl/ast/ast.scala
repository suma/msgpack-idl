package org.msgpack.idl.ast


case class Node
case class NodeType extends Node
case class TypeInt8 extends NodeType
case class TypeInt16 extends NodeType
case class TypeInt32 extends NodeType
case class TypeInt64 extends NodeType
case class TypeUInt8 extends NodeType
case class TypeUInt16 extends NodeType
case class TypeUInt32 extends NodeType
case class TypeUInt64 extends NodeType
case class TypeBoolean extends NodeType
case class TypeDouble extends NodeType
case class TypeByteArray extends NodeType
case class TypeString extends NodeType
case class TypeList(typ: NodeType) extends NodeType
case class TypeSet extends NodeType
case class TypeMap extends NodeType
case class TypeVoid extends NodeType
case class TypeSymbol(name: String) extends NodeType





