package org.zlox.zlox.Main

class LoxClass(val name: String, methods: Map[String, LoxFn]) extends LoxCallable {
  def arity: Int = findMethod("init").map(_.arity).getOrElse(0)
    
  def call(arguments: List[Any]): Any = {
    val instance = LoxInstance(this)
    val initializer = findMethod("init")
    initializer.map(_.bind(instance).call(arguments))
    return instance
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