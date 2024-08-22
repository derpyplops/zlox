package org.zlox.zlox.Main

class LoxClass(val name: String, methods: Map[String, LoxFn]) extends LoxCallable {
  def arity: Int = 0
    
  def call(arguments: List[Any]): Any = {
    LoxInstance(this)
  }

  def findMethod(name: String): Option[LoxFn] = {
    methods.get(name)
  }

  override def toString(): String = name
}

class LoxInstance(cls: LoxClass) {
  import collection.mutable.Map

  final val fields: Map[String, Any] = Map()

  def get(name: Token): Any = {
    fields.get(name.lexeme)
    .orElse(cls.findMethod(name.lexeme).map(_.bind(this)))
    .getOrElse(throw RuntimeError(name, "Undefined property '" + name.lexeme + "'."))
  }

  def set(name: Token, value: Any): Unit = {
    fields(name.lexeme) = value
  }
  override def toString(): String = cls.name + " instance"
}