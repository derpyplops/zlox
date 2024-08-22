package org.zlox.zlox.LoxClass

import org.zlox.zlox.Main.LoxCallable
import org.zlox.zlox.Main.Interpreter

class LoxClass(val name: String) extends LoxCallable {
  def arity: Int = 0
    
  def call(arguments: List[Any]): Any = {
    LoxInstance(this)
  }

  override def toString(): String = name
}

class LoxInstance(cls: LoxClass) {
  override def toString(): String = cls.name + " instance"
}