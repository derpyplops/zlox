package org.zlox.zlox.Main

import scala.collection.mutable

class LoxClass(val name: String, superclass: Option[LoxClass], methods: Map[String, LoxFn]) extends LoxCallable {
  def arity: Int = findMethod("init").map(_.arity).getOrElse(0)

  def call(arguments: List[Any]): Any = {
    val instance = LoxInstance(this)
    val initializer = findMethod("init")
    initializer.map(_.bind(instance).call(arguments))
    return instance
  }

  def findMethod(name: String): Option[LoxFn] = {
    methods.get(name).orElse(superclass.flatMap(_.findMethod(name)))
  }

  override def toString: String = name
}

class LoxInstance(cls: LoxClass) {
  import collection.mutable.Map

  private final val fields: mutable.Map[String, Any] = mutable.Map()

  def get(name: Token): Any = {
    fields.get(name.lexeme)
    .orElse(cls.findMethod(name.lexeme).map(_.bind(this)))
    .getOrElse(throw RuntimeError(name, "Undefined property '" + name.lexeme + "'."))
  }

  def set(name: Token, value: Any): Unit = {
    fields(name.lexeme) = value
  }
  override def toString: String = cls.name + " instance"
}