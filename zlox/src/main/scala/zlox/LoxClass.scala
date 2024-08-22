package org.zlox.zlox.Main

class LoxClass(val name: String) extends LoxCallable {
  def arity: Int = 0
    
  def call(arguments: List[Any]): Any = {
    LoxInstance(this)
  }

  override def toString(): String = name
}

class LoxInstance(cls: LoxClass) {
  import collection.mutable.Map

  final val fields: Map[String, Any] = Map()

  def get(name: Token): Any = {
    fields.getOrElse(name.lexeme, throw new RuntimeError(null, "Undefined property '" + name + "'."))
  }

  def set(name: Token, value: Any): Unit = {
    fields(name.lexeme) = value
  }
  override def toString(): String = cls.name + " instance"
}